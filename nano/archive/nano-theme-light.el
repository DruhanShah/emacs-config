;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
(require 'nano-base-colors)
(defun nano-theme-set-light ()
  (interactive)
  "Apply light Nano theme base."
  ;; Colors from Catppuccin Latte at https://github.com/catppuccin/palette
  (setq frame-background-mode    'light)
  (setq nano-color-foreground "#1e1e2e")
  (setq nano-color-background "#eff1f5")
  (setq nano-color-highlight  "#dce0e8")
  (setq nano-color-critical   "#fe640b")
  (setq nano-color-salient    "#8839ef")
  (setq nano-color-strong     "#11111b")
  (setq nano-color-popout     "#1e46d5") 
  (setq nano-color-subtle     "#cdd0da")
  (setq nano-color-faded      "#8c8fa1")
  ;; to allow for toggling of the themes.
  (setq nano-theme-var "light"))

(provide 'nano-theme-light)
