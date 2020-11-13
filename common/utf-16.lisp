(in-package #:unicode-common)


(defun code-point-encode-utf-16 (code-point)
  "Returns three values: code unit count, code unit 0, code unit 1."
  (check-type code-point scalar-value)
  (etypecase code-point
    (bmp-code-point
     (values 1 code-point))
    (t
     (values 2
             (logior +first-high-surrogate+
                       (1- (ldb (byte 5 16) code-point))
                       (ldb (byte 6 10) code-point))
             (logior +first-low-surrogate+
                     (ldb (byte 10 0) code-point))))))


(defmacro code-point-decode-utf-16 (&key ;; input forms
                                      (backtrackp       (error ":backtrackp is required"))
                                      (index-form       (error ":index-form is required"))
                                      (start-form       (when backtrackp (error ":start-form is required when backtrackp is true")))
                                      (end-form         (error ":end-form is required"))
                                      (ref-form         (error ":ref-form is required"))
                                      ;; output places
                                      (code-point-place (gensym "code-point-place-") code-point-place-p)
                                      (start-place      (gensym "start-place-")      start-place-p)
                                      (end-place        (gensym "end-place-")        end-place-p)
                                      (errorp-place     (gensym "errorp-place-")     errorp-place-p))
  "index-form, start-form and end-form are evaluated once, in that order.

ref-form is evaluated at macro expansion time with * bound to the index of the code unit to be accessed. Example: `(aref vector ,*)

backtrackp - when true, generates code that will backtrack at most 1 octets if index points in the middle of a subsequence.

index-form - position where decoding starts.

start-form - only used when backtrackp is true. Limits the backtracing to this position.

end-form - limits decoding to before this position.

code-point-place - the decoded code-point is stored here.

start-place - starting position of the subsequence. Always equal to or atmost 3 less than index-form.

end-place - next position after the subsequence.

errorp-place - true if subsequence was ill-formed. code-point is set
U+FFFD Replacement Character, start-place and end-place to the maximal
subpart of the ill-formed subsequence. Ref. Unicode 13.0 page 126.
"
  (with-gensyms (decode return-decode leading trailing index start end)
    (flet ((return-decode-error (code)
             `(progn (setf ,errorp-place ,code
                           ,code-point-place +replacement-character+)
                     (return-from ,decode)))
           (ref (*)
             (eval ref-form)))
      `(let (,@(unless code-point-place-p `(,code-point-place))
             ,@(unless start-place-p      `(,start-place))
             ,@(unless end-place-p        `(,end-place))
             ,@(unless errorp-place-p     `(,errorp-place))
             ,leading
             ,trailing
             (,index               ,index-form)
             ,@(when start-form
                 `((,start         ,start-form)))
             (,end                 ,end-form))
         (setf ,start-place        ,index
               ,end-place          (1+ ,start-place)
               ,code-point-place   ,(ref start-place)
               ,errorp-place       nil)
         (block ,decode

           ;; Non-surrogate?
           (when (typep ,code-point-place 'scalar-value)
             (return-from ,decode))

           (setf ,leading ,code-point-place)

           (flet ((,return-decode (,leading ,trailing)
                    (setf ,code-point-place (logior (ash  (1+ (ldb (byte  4 6) ,leading)) 16)
                                                    (ash      (ldb (byte  6 0) ,leading)  10)
                                                    (ldb (byte 10 0) ,trailing)))
                    (return-from ,decode)))

             ;; Does index point to high surrogate and is it not at the end?
             (when (and (typep ,leading 'high-surrogate)
                        (< (1+ ,start-place) ,end))
               (incf ,end-place)
               (,return-decode ,leading ,(ref `(1+ ,start-place))))

             ;; No, then index must point to low surrogate. Backtrack?
             (when (and ,backtrackp
                        (< ,start ,index))
               (setf ,trailing ,leading)
               (setf ,leading ,(ref `(1- ,start-place)))
               (unless (typep ,leading 'high-surrogate)
                 ,(return-decode-error :low-surrogate))
               (decf ,start-place)
               (,return-decode ,leading ,trailing)))

           ;; Otherwise it's ill-formed
           ,(return-decode-error :ill-formed))))))
