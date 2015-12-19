(defmacro withOpenFile (var filename mode ...)
  (return
    `(do (local ,var (io.open ,filename ,mode))
         (assert ,var (string.format "Unable to open file: %q" ,filename))
         (local (_ err) (pcall (function () ,@{...})))
         (io.close ,var)
         (if err (error err)))))
