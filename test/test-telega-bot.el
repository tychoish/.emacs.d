;;; test-telega-bot.el --- ERT tests for telega-bot.el -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'telega-bot)

(ert-deftest telega-bot/registry-and-creation ()
  "Test that bot creation automatically registers in `telega-bot-registry'."
  (let ((telega-bot-registry nil))
    (let ((bot (make-telega-bot :name "AutoBot" :token "tok-123")))
      (should (string-equal (telega-bot-name bot) "AutoBot"))
      (should (eq (telega-bot-get "AutoBot") bot))
      (should (eq (telega-bot-get 'AutoBot) bot))
      (should (eq (map-elt telega-bot-registry "AutoBot") bot))

      ;; Unregister
      (telega-bot-unregister "AutoBot")
      (should-not (telega-bot-get "AutoBot")))))

(ert-deftest telega-bot/register-handler-macro ()
  "Test the `telega-bot-register-handler' macro across all operations."
  (let ((telega-bot-registry nil))
    (let ((bot (make-telega-bot :name "MacroBot" :active t))
          (ran-cmd nil)
          (ran-cb nil)
          (ran-fuzzy nil)
          (ran-fsm nil))
      ;; 1. Define command
      (telega-bot-register-handler ping-cmd 'MacroBot
        :operation :command
        :pattern "/ping"
        (setq ran-cmd t))

      ;; 2. Define callback
      (telega-bot-register-handler click-btn 'MacroBot
        :operation :callback
        :pattern "click_now"
        :args (&key data)
        (setq ran-cb data))

      ;; 3. Define fuzzy / regex pattern
      (telega-bot-register-handler greet-pat "MacroBot"
        :operation :fuzzymatch
        :pattern "^[Hh]ello"
        :args (&key text)
        (setq ran-fuzzy text))

      ;; 4. Define FSM step
      (telega-bot-register-handler enter-age 'MacroBot
        :operation :step
        :state ask-age
        :args (&key text)
        (setq ran-fsm text))

      ;; Dispatch command
      (telega-bot-dispatch bot '(:@type "updateNewMessage"
                                 :message (:id 1 :chat_id 1 :sender_id (:user_id 2)
                                           :content (:@type "messageText" :text (:text "/ping")))))
      (should ran-cmd)

      ;; Dispatch callback
      (cl-letf (((symbol-function 'telega-server-send) (lambda (&rest _args) nil)))
        (telega-bot-dispatch bot `(:@type "updateNewCallbackQuery"
                                   :id "q1" :chat_id 1 :message_id 1 :sender_user_id 2
                                   :payload (:@type "callbackQueryPayloadData"
                                             :data ,(base64-encode-string "click_now" t))))
        (should (equal ran-cb "click_now")))

      ;; Dispatch fuzzy matcher
      (telega-bot-dispatch bot '(:@type "updateNewMessage"
                                 :message (:id 2 :chat_id 1 :sender_id (:user_id 2)
                                           :content (:@type "messageText" :text (:text "Hello friend")))))
      (should (equal ran-fuzzy "Hello friend"))

      ;; Dispatch FSM step
      (telega-bot-set-state bot 2 1 'ask-age)
      (telega-bot-dispatch bot '(:@type "updateNewMessage"
                                 :message (:id 3 :chat_id 1 :sender_id (:user_id 2)
                                           :content (:@type "messageText" :text (:text "25")))))
      (should (equal ran-fsm "25")))))

(ert-deftest telega-bot/multi-bot-generic-fsm-isolation ()
  "Test generic FSM methods specialized to specific bots in the registry."
  (let ((telega-bot-registry nil))
    (let ((bot-a (make-telega-bot :name "BotA" :active t))
          (bot-b (make-telega-bot :name "BotB" :active t))
          (res-a nil)
          (res-b nil))
      ;; Register identical step name for BotA and BotB with distinct behaviors
      (telega-bot-register-handler step-a 'BotA
        :operation :step
        :state ask-step
        :args (&key text)
        (setq res-a (concat "BotA got: " text)))

      (telega-bot-register-handler step-b 'BotB
        :operation :step
        :state ask-step
        :args (&key text)
        (setq res-b (concat "BotB got: " text)))

      ;; Set state for user in both bots
      (telega-bot-set-state bot-a 1 100 'ask-step)
      (telega-bot-set-state bot-b 1 200 'ask-step)

      ;; Dispatch to BotA
      (telega-bot-dispatch bot-a '(:@type "updateNewMessage"
                                   :message (:id 1 :chat_id 100 :sender_id (:user_id 1)
                                             :content (:@type "messageText" :text (:text "Apple")))))
      (should (equal res-a "BotA got: Apple"))
      (should-not res-b)

      ;; Dispatch to BotB
      (telega-bot-dispatch bot-b '(:@type "updateNewMessage"
                                   :message (:id 2 :chat_id 200 :sender_id (:user_id 1)
                                             :content (:@type "messageText" :text (:text "Banana")))))
      (should (equal res-b "BotB got: Banana")))))

(ert-deftest telega-bot/scoped-response-and-logging ()
  "Test that handlers can call `send-response` without managing chat/thread IDs."
  (let* ((bot (make-telega-bot :name "ScopedBot" :active t))
         (buf-name "*telega-bot-ScopedBot*")
         (sent-msg nil))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (cl-letf (((symbol-function 'telega-chat-get) (lambda (id) id))
              ((symbol-function 'telega-chat-send-message)
               (lambda (chat text &rest args)
                 (setq sent-msg (list chat text (map-elt args :reply-to-message-id)))
                 nil)))
      ;; Define command using scoped helper with no manual IDs
      (telega-bot-register-handler hello-cmd 'ScopedBot
        :operation :command
        :pattern "/hello"
        (send-response "Hello there!"))

      (let ((update '(:@type "updateNewMessage"
                      :message (:id 100
                                :chat_id 555
                                :message_thread_id 77
                                :sender_id (:user_id 888)
                                :content (:@type "messageText"
                                          :text (:text "/hello"))))))
        (telega-bot-dispatch bot update))

      ;; Should send to chat 555 and thread 77 automatically
      (should (equal sent-msg '(555 "Hello there!" 77)))

      ;; Check log buffer
      (with-current-buffer (get-buffer buf-name)
        (let ((content (buffer-string)))
          (should (string-match-p "Received message: /hello" content))
          (should (string-match-p "Sending reply to chat 555: Hello there!" content))))

      (kill-buffer buf-name))))

(ert-deftest telega-bot/fast-hash-and-fuzzy-command-dispatch ()
  "Test hash map O(1) command matching and fuzzy/case-insensitive matching."
  (let ((bot (make-telega-bot :name "FastBot" :active t))
        (cmd-called nil))
    (telega-bot-register-handler status-cmd 'FastBot
      :operation :command
      :pattern "/status"
      :args (&key text)
      (setq cmd-called text))

    ;; 1. Exact hash lookup
    (telega-bot-dispatch bot '(:@type "updateNewMessage"
                               :message (:id 1
                                         :chat_id 1
                                         :sender_id (:user_id 2)
                                         :content (:@type "messageText"
                                                   :text (:text "/status")))))
    (should (equal cmd-called "/status"))

    ;; 2. Suffix stripping (e.g. /status@MyBot)
    (setq cmd-called nil)
    (telega-bot-dispatch bot '(:@type "updateNewMessage"
                               :message (:id 2
                                         :chat_id 1
                                         :sender_id (:user_id 2)
                                         :content (:@type "messageText"
                                                   :text (:text "/status@FastBot check")))))
    (should (equal cmd-called "/status@FastBot check"))

    ;; 3. Case-insensitive lookup
    (setq cmd-called nil)
    (telega-bot-dispatch bot '(:@type "updateNewMessage"
                               :message (:id 3
                                         :chat_id 1
                                         :sender_id (:user_id 2)
                                         :content (:@type "messageText"
                                                   :text (:text "/STATUS")))))
    (should (equal cmd-called "/STATUS"))))

(ert-deftest telega-bot/auto-delete-keyboard-on-callback ()
  "Test that callbacks auto-delete their inline keyboards and stop spinners."
  (let ((bot (make-telega-bot :name "CbBot" :active t))
        (server-calls nil)
        (cb-ran nil))
    (telega-bot-register-handler approve-cmd 'CbBot
      :operation :callback
      :pattern "action_approve"
      (setq cb-ran t))

    (cl-letf (((symbol-function 'telega-server-send)
               (lambda (payload)
                 (push payload server-calls))))
      (let ((update `(:@type "updateNewCallbackQuery"
                      :id "query_999"
                      :chat_id 123
                      :message_id 456
                      :sender_user_id 789
                      :payload (:@type "callbackQueryPayloadData"
                                :data ,(base64-encode-string "action_approve" t)))))
        (telega-bot-dispatch bot update)))

      (should cb-ran)
      ;; Verify auto-answer and auto-delete keyboard were sent to server
      (let ((answer (cl-find-if (lambda (call) (string-equal (map-elt call :@type) "answerCallbackQuery")) server-calls))
            (delete-kb (cl-find-if (lambda (call) (string-equal (map-elt call :@type) "editMessageReplyMarkup")) server-calls)))
        (should answer)
        (should (string-equal (map-elt answer :callback_query_id) "query_999"))
        (should delete-kb)
        (should (equal (map-elt delete-kb :chat_id) 123))
        (should (equal (map-elt delete-kb :message_id) 456))
        (should (null (map-elt delete-kb :reply_markup))))))

(ert-deftest telega-bot/ask-yes-or-no-interaction ()
  "Test the `ask-yes-or-no` helper with callback triggering."
  (let ((bot (make-telega-bot :name "YonBot" :active t))
        (yes-triggered nil)
        (no-triggered nil)
        (sent-kb-rows nil))
    (cl-letf (((symbol-function 'telega-chat-get) (lambda (id) id))
              ((symbol-function 'telega-chat-send-message)
               (lambda (_chat _text &rest args)
                 (setq sent-kb-rows (map-elt (map-elt args :reply-markup) :rows))
                 nil))
              ((symbol-function 'telega-server-send) (lambda (&rest _args) nil)))
      (telega-bot-ask-yes-or-no "Delete file?"
        :bot bot
        :chat-id 321
        :on-yes (lambda () (setq yes-triggered t))
        :on-no (lambda () (setq no-triggered t)))

      (should sent-kb-rows)
      (let* ((btn-row (car sent-kb-rows))
             (yes-btn (car btn-row))
             (no-btn (cadr btn-row))
             (yes-data (decode-coding-string (base64-decode-string (map-elt (map-elt yes-btn :type) :data)) 'utf-8))
             (no-data (decode-coding-string (base64-decode-string (map-elt (map-elt no-btn :type) :data)) 'utf-8)))
        (should (string-equal (map-elt yes-btn :text) "Yes"))
        (should (string-equal (map-elt no-btn :text) "No"))

        ;; Simulate clicking Yes
        (telega-bot-dispatch bot `(:@type "updateNewCallbackQuery"
                                   :id "q1"
                                   :chat_id 321
                                   :message_id 11
                                   :sender_user_id 1
                                   :payload (:@type "callbackQueryPayloadData"
                                             :data ,(base64-encode-string yes-data t))))
        (should yes-triggered)
        (should-not no-triggered)

        ;; Reset and simulate clicking No
        (setq yes-triggered nil)
        (telega-bot-dispatch bot `(:@type "updateNewCallbackQuery"
                                   :id "q2"
                                   :chat_id 321
                                   :message_id 11
                                   :sender_user_id 1
                                   :payload (:@type "callbackQueryPayloadData"
                                             :data ,(base64-encode-string no-data t))))
        (should no-triggered)
        (should-not yes-triggered)))))

(ert-deftest telega-bot/activation-and-connection-lifecycle ()
  "Test bot activation and connection readiness latching by bot name and instance."
  (let ((bot (make-telega-bot :name "LifecycleBot"))
        (telega-ready-hook nil))
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil))
              ((symbol-function 'add-hook) (lambda (hook fn &rest _args)
                                             (when (eq hook 'telega-ready-hook)
                                               (push fn telega-ready-hook))))
              ((symbol-function 'remove-hook) (lambda (&rest _args) nil)))
      ;; Activate by name
      (telega-bot-activate "LifecycleBot")
      (should-not (telega-bot-active bot))

      ;; Trigger ready hook
      (run-hooks 'telega-ready-hook)
      (should (telega-bot-active bot))

      ;; Deactivate by name
      (telega-bot-deactivate "LifecycleBot")
      (should-not (telega-bot-active bot)))))

(ert-deftest telega-bot/no-dynamic-variables-lexical-scoping ()
  "Test that dynamic variables are removed and handlers rely purely on lexical scope."
  (should-not (boundp 'telega-bot--current-bot))
  (should-not (boundp 'telega-bot--current-chat-id))
  (should-not (boundp 'telega-bot--current-user-id))
  (should-not (boundp 'telega-bot--current-thread-id))
  (should-not (boundp 'telega-bot--current-msg-id))
  (should-not (boundp 'telega-bot--current-query-id))
  (should-not (boundp 'telega-bot--current-msg))
  (should-not (boundp 'telega-bot--current-update))

  (let ((bot (make-telega-bot :name "LexicalBot" :active t))
        (received-chat nil)
        (received-text nil)
        (received-user nil))
    (cl-letf (((symbol-function 'telega-chat-get) (lambda (id) id))
              ((symbol-function 'telega-chat-send-message)
               (lambda (chat text &rest _args)
                 (setq received-chat chat
                       received-text text)
                 nil)))
      (telega-bot-register-handler lex-cmd 'LexicalBot
        :operation :command
        :pattern "/lex"
        :args (&key text user-id)
        (setq received-user user-id)
        (send-response (format "Lexical: %s" text)))

      (telega-bot-dispatch bot '(:@type "updateNewMessage"
                                 :message (:id 42
                                           :chat_id 999
                                           :sender_id (:user_id 777)
                                           :content (:@type "messageText" :text (:text "/lex")))))

      (should (equal received-chat 999))
      (should (equal received-user 777))
      (should (equal received-text "Lexical: /lex")))))

(provide 'test-telega-bot)
;;; test-telega-bot.el ends here
