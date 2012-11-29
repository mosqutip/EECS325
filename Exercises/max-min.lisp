#|
Recursive defintion of max-min. This definition uses
a max and min key argument, passed in by successive calls
of the function to greatly simplify comparison, speeding
up the code.
|#
(defun max-min (v &key (start 0) (end (length v)) (max nil) (min nil))
  (cond ((or (= end 0) (= start end))
         (values max min))
        (t (let* ((current (svref v start))
                  (current-max (if (or (null max) (> current max))
                                   current 
                                 max))
                  (current-min (if (or (null min) (< current min))
                                   current
                                 min)))
             (max-min v :start (1+ start) :end end :max current-max :min current-min)))))


#|
Iterative definition of max-min.
|#
(defun max-min (v &key (start 0) (end (length v)))
  (if (or (= end 0) (= start end))
      (values nil nil)
    (do* ((i start (1+ i))
          (current (svref v i) (svref v i))
          (max current (if (> current max)
                           current
                         max))
          (min current (if (< current min)
                           current
                         min)))
         ((= i (1- end)) (values max min)))))