;;; telega-bot.el --- Bot engine in Emacs with Telega -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This library provides a high-level, event-driven Telegram Bot engine
;; built on top of Emacs Telega (TDLib).
;;
;; Handler Operations Overview:
;; ----------------------------
;; All handlers are registered using `telega-bot-register-handler' via
;; the `:operation' keyword:
;;
;;   1. `:command' (Slash Commands):
;;      Use for standard Telegram slash commands (e.g. "/start", "/ping").
;;      These are indexed in an O(1) hash map and automatically handle
;;      bot username suffixes (e.g. "/start@MyBot" matches "/start").
;;
;;   2. `:callback' (Inline Keyboard Clicks):
;;      Use for handling button presses from inline keyboards. Matches the
;;      callback data string or prefix. Automatically stops the Telegram client
;;      spinner and auto-deletes the inline keyboard from the message.
;;
;;   3. `:fuzzymatch' / `:fuzzy' (Pattern & Regex Matching):
;;      Use for matching arbitrary incoming text messages or callback data
;;      that are not fixed slash commands (e.g. regex "^[Hh]ello", natural
;;      language greetings, or prefix matching).
;;
;;   4. `:step' / `:state' (FSM Conversation States):
;;      Use for multi-step interactive workflows (e.g. asking a user for
;;      their email or confirmation). Dispatches via `cl-defgeneric' and
;;      `cl-defmethod' specialized on the bot and state symbol.
;;
;;   5. `:fallback':
;;      Use as a catch-all handler when no command, callback, or FSM step
;;      matches the incoming event.

;;; Usage Example:
;;
;;   (require 'telega-bot)
;;
;;   ;; 1. Define the bot instance (automatically registered in `telega-bot-registry`).
;;   (defvar my-echo-bot
;;     (make-telega-bot
;;      :name "EchoBot"
;;      :token "123456789:ABCdefGhIJKlmNoPQRsTUVwxyZ"))
;;
;;   ;; 2. Register a slash command:
;;   (telega-bot-register-handler ping-cmd 'EchoBot
;;     :operation :command
;;     :pattern "/ping"
;;     (send-response "pong"))
;;
;;   ;; 3. Register an interactive Yes/No confirmation:
;;   (telega-bot-register-handler confirm-cmd 'EchoBot
;;     :operation :command
;;     :pattern "/confirm"
;;     (ask-yes-or-no "Proceed with operation?"
;;       :on-yes (lambda () (send-response "Confirmed!"))
;;       :on-no  (lambda () (send-response "Aborted."))))
;;
;;   ;; 4. Register a regex / fuzzy matcher:
;;   (telega-bot-register-handler greeting 'EchoBot
;;     :operation :fuzzymatch
;;     :pattern "^[Hh]ello"
;;     :args (&key text)
;;     (send-response (format "Hello! You said: %s" text)))
;;
;;   ;; 5. Register an FSM step:
;;   (telega-bot-register-handler ask-name-step 'EchoBot
;;     :operation :step
;;     :state ask-name
;;     :args (&key text data)
;;     (send-response (format "Welcome, %s!" text))
;;     (clear-state))
;;
;;   ;; 6. Activate the bot:
;;   (telega-bot-activate 'EchoBot)

(require 'cl-lib)
(require 'subr-x)
(require 'telega)
(require 'telega-server)
(require 'telega-chat)
(require 'map)

;;; Registry

(defvar telega-bot-registry nil
  "Alist of active telega bot instances, keyed by bot name.")

;;; Core Structure

(cl-defstruct (telega-bot
               (:constructor make-telega-bot--raw)
               (:conc-name telega-bot-))
  "Structure representing a bot running on top of telega.el."
  (name "Telega Bot" :type string :documentation "The name of the telega bot instance.")
  (token nil :type (or null string) :documentation "The Telegram bot API authentication token.")
  (commands (make-hash-table :test 'equal) :documentation "Hash table mapping slash commands to handler functions.")
  (callbacks (make-hash-table :test 'equal) :documentation "Hash table mapping callback queries to handler functions.")
  (fuzzy-handlers nil :type list :documentation "List of fuzzy or pattern handlers.")
  (states (make-hash-table) :documentation "Hash table mapping FSM state symbols to handler functions.")
  (user-states (make-hash-table :test 'equal) :documentation "Hash table mapping (user-id . chat-id) to active (state-sym . data).")
  (fallback nil :type (or null function) :documentation "Fallback handler function called when no other handler matches.")
  (active nil :type boolean :documentation "Boolean flag indicating whether the bot is currently active and connected."))

(cl-defun make-telega-bot (&key (name "Telega Bot") token fallback active)
  "Create a new `telega-bot' and register it in `telega-bot-registry'."
  (let ((bot (make-telega-bot--raw
              :name name
              :token token
              :fallback fallback
              :active active)))
    (telega-bot-register bot name)
    bot))

(defun telega-bot-register (bot &optional name)
  "Register BOT in `telega-bot-registry' under NAME (defaults to bot's name)."
  (let ((key (or name (telega-bot-name bot))))
    (setf (map-elt telega-bot-registry (if (symbolp key) (symbol-name key) key)) bot)))

(defun telega-bot-get (name-or-bot)
  "Retrieve registered bot by NAME (string or symbol) or return BOT instance."
  (cond
   ((telega-bot-p name-or-bot) name-or-bot)
   ((symbolp name-or-bot)
    (or (map-elt telega-bot-registry (symbol-name name-or-bot))
        (and (boundp name-or-bot) (telega-bot-p (symbol-value name-or-bot)) (symbol-value name-or-bot))))
   ((stringp name-or-bot) (map-elt telega-bot-registry name-or-bot))))

(defun telega-bot-unregister (name-or-bot)
  "Remove bot by name or instance from `telega-bot-registry'."
  (let ((name (cond
               ((telega-bot-p name-or-bot) (telega-bot-name name-or-bot))
               ((symbolp name-or-bot) (symbol-name name-or-bot))
               (t name-or-bot))))
    (setq telega-bot-registry (map-delete telega-bot-registry name))))

(defun telega-bot--to-symbol (name-or-bot)
  "Normalize NAME-OR-BOT into an interned symbol for method specialization."
  (cond
   ((symbolp name-or-bot) (intern (symbol-name name-or-bot)))
   ((stringp name-or-bot) (intern name-or-bot))
   ((telega-bot-p name-or-bot) (intern (telega-bot-name name-or-bot)))
   (t (intern (format "%s" name-or-bot)))))

(defun telega-bot--macro-bot-expr (bot-or-name)
  "Generate runtime form evaluating BOT-OR-NAME."
  (cond
   ((stringp bot-or-name) bot-or-name)
   ((and (consp bot-or-name) (eq (car bot-or-name) 'quote)) bot-or-name)
   (t `(if (boundp ',bot-or-name) ,bot-or-name ',bot-or-name))))

(defun telega-bot--wrap-handler (fn)
  "Wrap FN so it accepts keyword arguments cleanly."
  (if (functionp fn)
      (lambda (&rest args)
        (condition-case nil
            (apply fn args)
          (wrong-number-of-arguments
           (funcall fn))))
    fn))

;;; Scoped Context Helpers for Implementors

(cl-defun telega-bot-send-response (text &key bot chat-id thread-id keyboard)
  "Send response TEXT to CHAT-ID with optional THREAD-ID and KEYBOARD."
  (when (and bot chat-id)
    (telega-bot--log bot "Sending reply to chat %s: %s" chat-id text))
  (ignore-errors
    (telega-chat-send-message
     (telega-chat-get chat-id) text
     :reply-to-message-id thread-id
     :reply-markup keyboard)))


(cl-defun telega-bot-send-keyboard (text rows &key bot thread-id chat-id)
  "Send TEXT with inline keyboard button ROWS."
  (telega-bot-send-response text
                            :bot bot
                            :keyboard (telega-bot--make-inline-keyboard rows)
                            :thread-id thread-id
                            :chat-id chat-id))


(cl-defun telega-bot-remove-keyboard (&key chat-id message-id)
  "Remove inline keyboard markup from CHAT-ID and MESSAGE-ID."
  (when (and chat-id message-id)
    (telega-server-send
     `(:@type "editMessageReplyMarkup"
       :chat_id ,chat-id
       :message_id ,message-id
       :reply_markup nil))))


(cl-defun telega-bot-answer-callback (query-id text &key bot alert)
  "Answer callback query QUERY-ID with TEXT and optional ALERT toast."
  (when query-id
    (when bot
      (telega-bot--log bot "Answering callback %s (alert=%s): %s" query-id alert text))
    (telega-server-send
     `(:@type "answerCallbackQuery"
       :callback_query_id ,query-id
       :text ,text
       :show_alert ,(if alert t json-false)))))

(cl-defun telega-bot-ask-yes-or-no (question &key
                                             on-yes on-no
                                             (yes-label "Yes")
                                             (no-label "No")
                                             bot
                                             chat-id
                                             thread-id)
  "Send a Yes/No question with interactive buttons that auto-delete on click."
  (let* ((uniq (format "%d_%d" (time-convert nil 'integer) (random 100000)))
         (yes-data (concat "yon_y_" uniq))
         (no-data (concat "yon_n_" uniq)))
    (when bot
      (setf (map-elt (telega-bot-callbacks bot) yes-data)
            (lambda (&rest _args)
              (when on-yes (funcall on-yes))))
      (setf (map-elt (telega-bot-callbacks bot) no-data)
            (lambda (&rest _args)
              (when on-no (funcall on-no)))))
    (telega-bot-send-keyboard question
                              `(((,yes-label . ,yes-data) (,no-label . ,no-data)))
                              :bot bot
                              :chat-id chat-id
                              :thread-id thread-id)))

;;; Functional Handler Registration

(cl-defun telega-bot--register-handler (symbol bot-or-name &key (operation :command) pattern state fn &allow-other-keys)
  "Internal functional dispatcher for registering handler SYMBOL for BOT-OR-NAME."
  (let ((bot (telega-bot-get bot-or-name)))
    (unless bot
      (error "Bot %s not found in registry" bot-or-name))
    (let ((wrapped-fn (telega-bot--wrap-handler fn))
          (op (cond
               ((memq operation '(:command command)) :command)
               ((memq operation '(:callback callback)) :callback)
               ((memq operation '(:fuzzymatch fuzzymatch :fuzzy fuzzy)) :fuzzy)
               ((memq operation '(:step step :state state)) :step)
               ((memq operation '(:fallback fallback)) :fallback)
               (t (error "Unknown handler operation: %s" operation)))))
      (pcase op
        (:command
         (let ((cmd (or pattern (concat "/" (symbol-name symbol)))))
           (setf (map-elt (telega-bot-commands bot) cmd) wrapped-fn)))
        (:callback
         (let ((cb (or pattern (symbol-name symbol))))
           (setf (map-elt (telega-bot-callbacks bot) cb) wrapped-fn)))
        (:fuzzy
         (let ((pat (or pattern (symbol-name symbol))))
           (push (list :id symbol :type :msg :pattern pat :fn wrapped-fn)
                 (telega-bot-fuzzy-handlers bot))))
        (:step
         (let ((st (or state symbol)))
           (setf (map-elt (telega-bot-states bot) st) wrapped-fn)))
        (:fallback
         (setf (telega-bot-fallback bot) wrapped-fn))))))

;;; Macro Handler Registration

(defun telega-bot--parse-macro-args (args)
  "Parse ARGS into (has-bot-pos . key-bindings)."
  (let* ((first (car args))
         (has-bot (and first (not (eq first '&key)) (symbolp first)))
         (keys-start (if has-bot (cdr args) args))
         keys seen-key)
    (dolist (item keys-start)
      (cond
       ((eq item '&key) (setq seen-key t))
       (seen-key (push item keys))))
    (cons has-bot (nreverse keys))))

(defun telega-bot--wrap-body (body bot-expr rest-args-var has-bot bot-arg-name keys)
  "Wrap BODY in a lexical environment providing context variables and helpers."
  (let ((bot-var (gensym "bot"))
        (chat-var (gensym "chat-id"))
        (user-var (gensym "user-id"))
        (thread-var (gensym "thread-id"))
        (msg-var (gensym "msg-id"))
        (query-var (gensym "query-id"))
        (m-var (gensym "msg"))
        (u-var (gensym "update"))
        (t-var (gensym "text"))
        (d-var (gensym "data")))
    `(let* ((,bot-var (or (map-elt ,rest-args-var :bot) ,bot-expr))
            (,chat-var (map-elt ,rest-args-var :chat-id))
            (,user-var (map-elt ,rest-args-var :user-id))
            (,thread-var (map-elt ,rest-args-var :thread-id))
            (,msg-var (map-elt ,rest-args-var :msg-id))
            (,query-var (map-elt ,rest-args-var :query-id))
            (,m-var (map-elt ,rest-args-var :msg))
            (,u-var (map-elt ,rest-args-var :update))
            (,t-var (or (map-elt ,rest-args-var :text) ""))
            (,d-var (map-elt ,rest-args-var :data))
            (bot ,bot-var)
            (chat-id ,chat-var)
            (user-id ,user-var)
            (thread-id ,thread-var)
            (msg-id ,msg-var)
            (query-id ,query-var)
            (msg ,m-var)
            (update ,u-var)
            (text ,t-var)
            (data ,d-var)
            ,@(when (and has-bot bot-arg-name (not (eq bot-arg-name 'bot)))
                `((,bot-arg-name ,bot-var)))
            ,@(mapcar (lambda (k)
                        `(,k (map-elt ,rest-args-var ,(intern (concat ":" (symbol-name k))))))
                      (if has-bot (remq bot-arg-name keys) keys)))
       (cl-flet ((send-response (text-arg &key keyboard (bot ,bot-var) (thread-id ,thread-var) (chat-id ,chat-var) &allow-other-keys)
                   (telega-bot-send-response text-arg :bot bot :chat-id chat-id :thread-id thread-id :keyboard keyboard))
                 (send-keyboard (text-arg rows-arg &key (bot ,bot-var) (thread-id ,thread-var) (chat-id ,chat-var) &allow-other-keys)
                   (telega-bot-send-keyboard text-arg rows-arg :bot bot :thread-id thread-id :chat-id chat-id))
                 (remove-keyboard (&key (chat-id ,chat-var) (message-id ,msg-var) &allow-other-keys)
                   (telega-bot-remove-keyboard :chat-id chat-id :message-id message-id))
                 (answer-callback (text-arg &key alert (bot ,bot-var) (query-id ,query-var) &allow-other-keys)
                   (telega-bot-answer-callback query-id text-arg :bot bot :alert alert))
                 (set-state (st-arg &key data (bot ,bot-var) (user-id ,user-var) (chat-id ,chat-var) &allow-other-keys)
                   (telega-bot-set-state bot user-id chat-id st-arg :data data))
                 (get-state (&optional (b ,bot-var) (u ,user-var) (c ,chat-var))
                   (telega-bot-get-state b u c))
                 (clear-state (&optional (b ,bot-var) (u ,user-var) (c ,chat-var))
                   (telega-bot-clear-state b u c))
                 (ask-yes-or-no (q-arg &key on-yes on-no (yes-label "Yes") (no-label "No") (bot ,bot-var) (chat-id ,chat-var) (thread-id ,thread-var) &allow-other-keys)
                   (telega-bot-ask-yes-or-no q-arg :on-yes on-yes :on-no on-no :yes-label yes-label :no-label no-label :bot bot :chat-id chat-id :thread-id thread-id)))
         ,@body))))

(cl-defmacro telega-bot-register-handler (name bot-or-name &rest spec &key (operation :command) pattern state args &allow-other-keys)
  "Register handler NAME for BOT-OR-NAME."
  (declare (indent 2))
  (let* ((body (cl-loop for item on spec by #'cddr
                        while (keywordp (car item))
                        finally return item))
         (parsed (telega-bot--parse-macro-args args))
         (has-bot (car parsed))
         (keys (cdr parsed))
         (bot-arg-name (when has-bot (car args)))
         (bot-expr (telega-bot--macro-bot-expr bot-or-name))
         (rest-var (gensym "rest-args"))
         (wrapped-body (telega-bot--wrap-body body bot-expr rest-var has-bot bot-arg-name keys))
         (lambda-form `(lambda (&rest ,rest-var)
                         ,wrapped-body))
         (clean-op (cond
                    ((memq operation '(:step step :state state)) :step)
                    ((memq operation '(:callback callback)) :callback)
                    ((memq operation '(:fuzzymatch fuzzymatch :fuzzy fuzzy)) :fuzzy)
                    ((memq operation '(:fallback fallback)) :fallback)
                    (t :command))))
    (if (eq clean-op :step)
        (let* ((clean-state (if (and (consp (or state name)) (eq (car (or state name)) 'quote))
                                (cadr (or state name))
                              (or state name)))
               (bot-sym (cond
                         ((and (consp bot-or-name) (eq (car bot-or-name) 'quote)) (cadr bot-or-name))
                         ((symbolp bot-or-name) (intern (symbol-name bot-or-name)))
                         ((stringp bot-or-name) (intern bot-or-name))
                         (t (intern (format "%s" bot-or-name))))))
          `(progn
             (telega-bot--register-handler ',name ,bot-expr
                                           :operation :step
                                           :state ',clean-state
                                           :fn ,lambda-form)
             (cl-defmethod telega-bot-handle-state ((_bot-id (eql ,bot-sym))
                                                     (_state (eql ,clean-state))
                                                     _text
                                                     &rest ,rest-var)
               ,wrapped-body)))
      `(telega-bot--register-handler ',name ,bot-expr
                                     :operation ',clean-op
                                     :pattern ,pattern
                                     :fn ,lambda-form))))

;;; Logging Helper

(defun telega-bot--log (bot format-string &rest args)
  "Log a message to the *telega-bot-<name>* buffer."
  (let ((buf-name (format "*telega-bot-%s*" (telega-bot-name bot))))
    (with-current-buffer (get-buffer-create buf-name)
      (save-excursion
        (goto-char (point-max))
        (insert (apply #'format format-string args) "\n")))))

;;; FSM State Management

(cl-defun telega-bot-set-state (bot user-id chat-id state &key data)
  "Set active conversation STATE for USER-ID in CHAT-ID with optional DATA."
  (setf (map-elt (telega-bot-user-states bot) (cons user-id chat-id)) (cons state data)))

(defun telega-bot-get-state (bot user-id chat-id)
  "Get active state (state-sym . data) for USER-ID in CHAT-ID."
  (map-elt (telega-bot-user-states bot) (cons user-id chat-id)))

(defun telega-bot-clear-state (bot user-id chat-id)
  "Clear active FSM state for USER-ID in CHAT-ID."
  (map-delete (telega-bot-user-states bot) (cons user-id chat-id)))

;;; Messaging, Keyboards & Authentication

(cl-defun telega-bot-reply (chat-id text &key thread-id keyboard)
  "Send TEXT to CHAT-ID with optional forum THREAD-ID and inline KEYBOARD."
  (telega-bot-send-response text :chat-id chat-id :thread-id thread-id :keyboard keyboard))

(defun telega-bot--make-inline-keyboard (rows)
  "Build inline keyboard payload from button ROWS: (((\"Text\" . \"data\")))."
  `(:@type "replyMarkupInlineKeyboard"
    :rows ,(mapcar
            (lambda (row)
              (mapcar
               (lambda (btn)
                 `(:@type "inlineKeyboardButton"
                   :text ,(car btn)
                   :type (:@type "inlineKeyboardButtonTypeCallback"
                          :data ,(base64-encode-string
                                  (encode-coding-string (cdr btn) 'utf-8) t))))
               row))
            rows)))

(defalias 'telega-bot-make-inline-keyboard 'telega-bot--make-inline-keyboard)


(cl-defun telega-bot-login (bot &key token)
  "Authenticate active TDLib session using BOT token."
  (when-let* ((auth-token (or token (telega-bot-token bot))))
    (setf (telega-bot-token bot) auth-token)
    (telega-bot--log bot "Logging in with token: %s" auth-token)
    (telega-server-send
     `(:@type "checkAuthenticationToken"
       :token ,auth-token))))

;;; Generic FSM State Handler

(cl-defgeneric telega-bot-handle-state (bot-id state text &key bot data msg thread-id &allow-other-keys)
  "Generic FSM state method handler for BOT-ID in STATE with input TEXT.")

(cl-defmethod telega-bot-handle-state (_bot-id _state _text &key &allow-other-keys)
  "Default FSM state handler when no specialized method matches."
  nil)

;;; Fast Dispatch Engine & Fuzzy Matching

(defun telega-bot--extract-command (text)
  "Extract the slash command from TEXT, stripping bot username suffix if present."
  (when (and text (string-prefix-p "/" text))
    (let* ((first-word (car (split-string text " " t)))
           (at-pos (string-match-p "@" first-word)))
      (if at-pos
          (substring first-word 0 at-pos)
        first-word))))

(defun telega-bot--find-message-handler (bot text)
  "Find message handler using exact slash-command hash lookup, with fuzzy/regex fallback."
  (or
   ;; 1. Slash command lookup
   (when-let* ((cmd (telega-bot--extract-command text)))
     (or
      ;; Exact hash map lookup O(1)
      (map-elt (telega-bot-commands bot) cmd)
      ;; Case-insensitive / lowercase match
      (map-elt (telega-bot-commands bot) (downcase cmd))
      ;; Prefix command matching
      (catch 'found
        (map-do (lambda (registered-cmd fn)
                  (when (or (string-prefix-p registered-cmd text t)
                            (string-prefix-p (downcase registered-cmd) (downcase text)))
                    (throw 'found fn)))
                (telega-bot-commands bot)))))
   ;; 2. Fuzzy / regex message handlers
   (catch 'found
     (dolist (handler (telega-bot-fuzzy-handlers bot))
       (when (and (eq (map-elt handler :type) :msg)
                  (or (string-prefix-p (map-elt handler :pattern) text t)
                      (string-match-p (map-elt handler :pattern) text)))
         (throw 'found (map-elt handler :fn)))))))

(defun telega-bot--find-callback-handler (bot data)
  "Find callback handler using exact hash map lookup with prefix/fuzzy fallback."
  (or
   ;; 1. Exact hash map lookup O(1)
   (map-elt (telega-bot-callbacks bot) data)
   ;; 2. Prefix / pattern matching
   (catch 'found
     (map-do (lambda (prefix fn)
               (when (string-prefix-p prefix data)
                 (throw 'found fn)))
             (telega-bot-callbacks bot))
     (dolist (handler (telega-bot-fuzzy-handlers bot))
       (when (and (eq (map-elt handler :type) :cb)
                  (or (string-prefix-p (map-elt handler :pattern) data)
                      (string-match-p (map-elt handler :pattern) data)))
         (throw 'found (map-elt handler :fn)))))))

(defun telega-bot--dispatch-message (bot update)
  "Route incoming messages to FSM steps, commands, or fallback."
  (when-let* ((msg (map-elt update :message))
              ((not (map-elt msg :is_outgoing)))
              (content (map-elt msg :content))
              ((string-equal (map-elt content :@type) "messageText"))
              (text (map-elt (map-elt content :text) :text))
              (chat-id (map-elt msg :chat_id))
              (user-id (map-elt (map-elt msg :sender_id) :user_id)))
    (let* ((thread-id (map-elt msg :message_thread_id))
           (msg-id (map-elt msg :id))
           (bot-sym (telega-bot--to-symbol bot)))
      (telega-bot--log bot "Received message: %s (chat %s, user %s)" text chat-id user-id)
      (cond
       ;; 1. Optional FSM Intercept via Generics (`cl-defgeneric` / `cl-defmethod`) or states table
       ((when-let* ((user-st (telega-bot-get-state bot user-id chat-id)))
          (let ((state-sym (car user-st)))
            (or
             ;; A. Try generic method specialized on bot-id and state
             (telega-bot-handle-state
              bot-sym state-sym text
              :text text
              :bot bot
              :data (cdr user-st)
              :thread-id thread-id
              :msg-id msg-id
              :msg msg
              :chat-id chat-id
              :user-id user-id)
             ;; B. Try registered function in states table
             (when-let* ((fn (map-elt (telega-bot-states bot) state-sym)))
              (funcall fn :bot bot :text text :data (cdr user-st) :thread-id thread-id :msg-id msg-id :msg msg :chat-id chat-id :user-id user-id)
               t)
             t))))

       ;; 2. Command Matcher via Hash Table & Fuzzy Search
       ((when-let* ((handler (telega-bot--find-message-handler bot text)))
         (funcall handler :bot bot :text text :thread-id thread-id :msg-id msg-id :msg msg :chat-id chat-id :user-id user-id)
          t))

       ;; 3. Fallback
       ((telega-bot-fallback bot)
        (funcall (telega-bot-fallback bot) :bot bot :text text :thread-id thread-id :msg-id msg-id :msg msg :chat-id chat-id :user-id user-id))))))

(defun telega-bot--dispatch-callback (bot update)
  "Route inline keyboard button presses, auto-answering and auto-deleting keyboards."
  (when-let* ((query-id (map-elt update :id))
              (chat-id (map-elt update :chat_id))
              (user-id (map-elt update :sender_user_id))
              (msg-id (map-elt update :message_id))
              (payload (map-elt update :payload))
              ((string-equal (map-elt payload :@type) "callbackQueryPayloadData"))
              (data (decode-coding-string (base64-decode-string (map-elt payload :data)) 'utf-8))
              (handler (telega-bot--find-callback-handler bot data)))
    (let* ()
      (telega-bot--log bot "Received callback: %s (query %s, chat %s, user %s)" data query-id chat-id user-id)
      ;; 1. Auto-answer callback so Telegram client spinner stops
      (telega-bot-answer-callback query-id "" :bot bot)
      ;; 2. Auto-delete the inline keyboard after use
      (telega-bot-remove-keyboard :chat-id chat-id :message-id msg-id)
      ;; 3. Execute the callback handler with pure keyword arguments
      (funcall handler :bot bot :data data :query-id query-id :chat-id chat-id :user-id user-id :msg-id msg-id :update update))))

(defun telega-bot-dispatch (bot update)
  "Main event router using `cond` branching."
  (when-let* ((_ (telega-bot-active bot))
              (type (map-elt update :@type)))
    (cond
     ((string-equal type "updateNewMessage")
      (telega-bot--dispatch-message bot update))
     ((string-equal type "updateNewCallbackQuery")
      (telega-bot--dispatch-callback bot update)))))

;;; Hook & Registry Management

(defun telega-bot--ready-handler ()
  "Callback for `telega-ready-hook` to mark bots active."
  (dolist (cell telega-bot-registry)
    (let ((bot (cdr cell)))
      (setf (telega-bot-active bot) t))))

(defun telega-bot--global-handler (update)
  "Hook listener dispatching updates to registered bots."
  (dolist (cell telega-bot-registry)
    (when-let* ((bot (cdr cell)))
      (telega-bot-dispatch bot update))))

(defun telega-bot-activate (bot-or-name)
  "Activate bot by instance or name and attach global update hook."
  (let ((bot (telega-bot-get bot-or-name)))
    (unless bot
      (error "Bot %s not found in registry" bot-or-name))
    (telega-bot-register bot)
    (add-hook 'telega-server-handler-functions #'telega-bot--global-handler)
    (if (telega-server-live-p)
        (setf (telega-bot-active bot) t)
      (add-hook 'telega-ready-hook #'telega-bot--ready-handler))))

(defun telega-bot-deactivate (bot-or-name)
  "Deactivate bot by instance or name and detach global update hook if registry is empty."
  (let ((bot (telega-bot-get bot-or-name)))
    (when bot
      (setf (telega-bot-active bot) nil)
      (telega-bot-unregister bot)
      (unless telega-bot-registry
        (remove-hook 'telega-server-handler-functions #'telega-bot--global-handler)
        (remove-hook 'telega-ready-hook #'telega-bot--ready-handler)))))

(provide 'telega-bot)
;;; telega-bot.el ends here
