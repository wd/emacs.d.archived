(require 'cl-lib)

(defun company-react (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-react))
    (prefix (when (looking-back "com\\>")
              (match-string 0)))
    (candidates (when (equal arg "com")
                  (list
                   "constructor"
                   "componentDidMount"
                   "componentWillMount"
                   "componentWillReceiveProps"
                   "componentWillUpdate"
                   "componentWillUnmount"
                   "shouldComponentUpdate"
                   "componentDidUpdate")))
    (meta (format "%s" arg))))

(provide 'company-my-backend)
