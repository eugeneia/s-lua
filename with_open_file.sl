(defmacro withOpenFile (var filename mode ...)
  (return
    `(do (local ,var (io.open ,filename ,mode))
         (local (status err) (pcall (function () ,@{...})))
         (io.close ,var)
         (if err
             (error err)
             (return status)))))
