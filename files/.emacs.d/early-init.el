;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No menu-bar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No tool-bar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No alarms by default
(setq ring-bell-function 'ignore)
