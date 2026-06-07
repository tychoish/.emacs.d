(use-package beads
  :defer t
  :load-path "extenral/beads"
  :commands (beads-list beads-menu beads-create-issue)
  :init
  (bind-keys
   :map tychoish/robot-map
   :prefix "b"
   :prefix-map tychoish/robot-beads-map
   ("l" . beads-list)
   ("n" . beads-create-issue)
   ("m" . beads-menu))
  (make-read-extended-command-for-prefix "beads"
    :bind-map tychoish/robot-beads-map
    :bind-key "x"))

