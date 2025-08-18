;;; 20-llm.el --- AI shell & tooling -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker"))

(use-package chatgpt-shell
  :straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
  :custom
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))
  (chatgpt-shell-anthropic-key (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))))

(provide '20-llm)

;;; 20-llm.el ends here
