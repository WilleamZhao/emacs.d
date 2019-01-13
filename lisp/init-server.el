;; define function to shutdown emacs server instance
(defun* server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

(provide 'init-server)
