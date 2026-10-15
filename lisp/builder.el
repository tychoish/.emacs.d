;;; builder.el --- Forwarding shim for standalone builder package -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0

;;; Commentary:
;; Forwarding shim to ~/src/builder until fully installed via package-install.

;;; Code:

(add-to-list 'load-path (expand-file-name "~/src/builder"))
(require 'builder)

(provide 'builder)
;;; builder.el ends here
