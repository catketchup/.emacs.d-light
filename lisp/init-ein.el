;; init-ein.e. --- Initialize ipython ein mode configuration

(use-package ein
  :config
  (setq ein:use-auto-complete t)
  (setq ein:use-smartrep t)
  :hook (ein:ipdb-mode . auto-save-mode)
  )
(provide 'init-ein)
