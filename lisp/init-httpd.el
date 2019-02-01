;; httpd
(require-package 'simple-httpd)

(setq httpd-port 8888)
(setq httpd-root "/Users/sourcod/workspace/org/blog/publish/")
(require 'simple-httpd)
(defservlet hello-world text/plain (path)
  (insert "hello, " (file-name-nondirectory path)))

;;(defservlet test "text/plain;charset=utf-8" (path)
  ;;(insert "测试成功!") (file-name-nondirectory path))
(httpd-start)
(provide 'init-httpd)
