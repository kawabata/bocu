;;; bocu-test.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Taichi Kawabata

;; Author: Taichi Kawabata <kawabata.taichi@gmail.com>
;; Keywords: i18n, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test of bocu.el

;;; Code:

(require 'ert)
(require 'bocu)

(ert-deftest bocu-hello ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "HELLO" data-directory))
    (let* ((orig (buffer-substring (point-min) (point-max)))
           (encode (bocu-from-string orig))
           (decode (bocu-to-string encode)))
      (should (string= orig decode)))))

(provide 'bocu-test)

;;; bocu-test.el ends here
