;;; 2dg-path.el --- geometry path helpers -*- lexical-binding: t -*-

;;; Commentary:
;; A 2dg-path represents an ordered collection of contiguous segments
;; stored internally as a collection of points in a connect-the-dots
;; style system.  A valid path may contain no points/segments when it
;; is empty.
;;
;; An 2dg-cardinal-path represents a 2dg-path where each segment of
;; the path must be a purely horizontal or vertical line (a line in a
;; cardinal direction).

;;; Code:
(require 'eieio)
(require '2dg-point)
(require '2dg-segment)

(defclass 2dg-path ()
  ((points :initarg :points
           :accessor 2dg-points
           :initform '()
           :type (or null list)))
  ;; TODO - Should there be a make-instance for this class which
  ;; validates that all things in the points slot are actually points?
  ;; If so, would there be some way to circumvent that check when it's
  ;; called by a trustworthy source such as the make-instance for
  ;; 2dg-cardinal-path?
)
(defclass 2dg-cardinal-path (2dg-path)
  ())
(defun 2dg-cardinal-path-class-p (any)
  "equivalent of (object-of-class-p ANY '2dg-cardinal-path)"
  (object-of-class-p any '2dg-cardinal-path))
(defsubst 2dg-is-cardinal-pts-list-p (points-list)
  "Return non-nil if POINTS-LIST contains a list of cardinal path points."
  (cl-loop with last-pt = (first points-list)
           with is-cardinal = 't
           for pt in (cdr points-list)
           when pt
           do (setq is-cardinal (2dg-cardinal-displacement-p pt last-pt))
           do (setq last-pt pt)
           until (not is-cardinal)
           finally return is-cardinal))
(cl-defmethod make-instance ((class (subclass 2dg-cardinal-path)) &rest slots)
  "Ensure the points provided are truly a cardinal path before making the instance."
  ;; TODO - add a toggle here to skip the check from trustworthy callers
  ;; TODO - should this be a :before method?
  (let ((points (plist-get slots :points)))
    (unless (or (null points)
                (2dg-is-cardinal-pts-list-p points))
      (error "An 2dg-cardinal-path must have only vertical or horizontal segments")))
  (cl-call-next-method))

(cl-defmethod 2dg-pprint ((path 2dg-path))
  "Return a stringified version of PATH for human eyes."
  (with-slots (points) path
    (format "%s[%s]"
            (if (2dg-cardinal-path-p path)
                "cp"
              "p")
            (mapconcat #'2dg-pprint points ", "))))
(cl-defmethod cl-print-object ((object 2dg-path) stream)
  "Pretty print the OBJECT to STREAM."
  (princ (2dg-pprint object) stream))
(cl-defmethod 2dg-num-points ((path 2dg-path))
  "Return the number of points that make up PATH."
  (length (oref path points)))
(cl-defgeneric 2dg-nth ((path 2dg-path) (N number))
  "Return the N-th point from PATH.")
(cl-defmethod 2dg-nth ((path 2dg-path) (N number))
  "Return the N-th point from PATH."
  (nth N (oref path points)))
(cl-defmethod 2dg-end ((path 2dg-path))
  "Return the last point in PATH."
  (2dg-nth path (1- (2dg-num-points path))))
(cl-defmethod 2dg-start ((path 2dg-path))
  "Return the first point in PATH."
  (2dg-nth path 0))
(cl-defmethod 2dg-start-vector ((path 2dg-path))
  "Return a characteristic-vector of PATH's first segment.

May return nil if PATH contains 1 or less points."
  (let ((second (2dg-nth path 1)))
    (when second
      (2dg-subtract second (2dg-nth path 0)))))
(cl-defmethod 2dg-end-vector ((path 2dg-path))
  "Return a characteristic-vector of PATH's last segment.

May return nil if PATH contains 1 or less points."
  (let ((num-pts (2dg-num-points path)))
    (when (> num-pts 1)
      (2dg-subtract (2dg-nth path (1- num-pts))
                      (2dg-nth path (- num-pts 2))))))
(cl-defmethod 2dg-push ((path 2dg-path) (pt 2dg-point))
  "Modify PATH by pushing PT on to the start."
  (push pt (2dg-points path)))
(defsubst 2dg--path-pts-truncate-1 (points-list)
  "Remove 1 point from the start and one point from the end of POINTS-LIST."
  (butlast (cdr points-list)))
(defsubst 2dg--path-list-truncate (points start-points end-points)
  "Return a copy of POINTS with START-POINTS and END-POINTS trimmed off the start/end."
  (butlast (nthcdr start-points points) end-points))
(cl-defgeneric 2dg-truncate (path-or-pts (start-points integer) (end-points integer))
  "Return a new path consisting of the points of PATH minus START-POINTS removed from the start and END-POINTS removed from the end.")
(cl-defmethod 2dg-truncate ((path-pts list) (start-points integer) (end-points integer))
  "Return a new list consisting of the points of PATH-PTS minus START-POINTS removed from the start and END-POINTS removed from the end."
  (2dg--path-list-truncate path-pts start-points end-points))
(cl-defmethod 2dg-truncate ((path 2dg-path) (start-points integer) (end-points integer))
  "Return a new 2dg-path consisting of the points of PATH minus START-POINTS removed from the start and END-POINTS removed from the end."
  (with-slots (points) path
    (2dg-path :points (2dg--path-list-truncate points start-points end-points))))
(cl-defmethod 2dg-truncate ((path 2dg-cardinal-path) (start-points integer) (end-points integer))
  "Return a new 2dg-cardinal-path consisting of the points of PATH minus START-POINTS removed from the start and END-POINTS removed from the end."
  (with-slots (points) path
    (2dg-cardinal-path :points (2dg--path-list-truncate points start-points end-points))))

(cl-defmethod 2dg-almost-equal ((A 2dg-path) (B 2dg-path) &optional tolerance)
  "Return non-nil if A and B are equal within TOLERANCE."
  (let ((a-len (2dg-num-points A))
        (b-len (2dg-num-points B)))
    (if (not (eq a-len b-len))
        'nil
      (let ((i 0)
            (miss-match 'nil))
        (while (and (not miss-match)
                    (< i a-len))
          (when (not (2dg-almost-equal (2dg-nth A i)
                                         (2dg-nth B i)
                                         tolerance))
            (setq miss-match 't))
          (incf i))
        (not miss-match)))))
(cl-defmethod 2dg-segments ((path 2dg-path))
  "Return an ordered list of 2dg-segment objects describing PATH.

May return nil if PATH has less than 2 points."
  (let* ((points (2dg-points path))
         (last-point (car points)))
    (mapcar (lambda (pt)
              (let ((segment (2dg-segment :start last-point
                                            :end pt)))
                (setq last-point pt)
                segment))
            (cdr points))))
(cl-defmethod 2dg-length ((path 2dg-path))
  "Return the length of the path (Not the displacement or number of points).

Not to be confused with 2dg-num-points.  This is the same as
adding up the length of every segment."
  (with-slots (points) path
    (let ((last-pt (first points))
          (len 0.0))
      (mapc (lambda (pt)
              (incf len (2dg-distance last-pt pt))
              (setq last-pt pt))
            (cdr points))
      len)))
(cl-defmethod 2dg-pierced-p ((A 2dg-path) (B 2dg-path) &optional allow-A-start allow-A-end allow-B-start allow-B-end)
  "Return non-nil if A pierces B at some point.

End points are considered by their flags."
  (let* ((a-segments (2dg-segments A))
         (b-segments (2dg-segments B))
         (a-first (car a-segments))
         (a-last (last a-segments))
         (b-first (car b-segments))
         (b-last (last b-segments)))
    (block 2dg--collision
      (mapc                             ;A -map
       (lambda (a-segment)
         (let ((allow-sa-min (or allow-A-start (not (eq a-first a-segment))))
               (allow-sa-max (or allow-A-end (not (eq a-last a-segment)))))
           (mapc                        ;B -map
            (lambda (b-segment)
              (let ((allow-sb-min (or allow-B-start (not (eq b-first b-segment))))
                    (allow-sb-max (or allow-B-end (not (eq b-last b-segment)))))
                (when (2dg-pierced-p a-segment
                                      b-segment
                                      allow-sa-min
                                      allow-sa-max
                                      allow-sb-min
                                      allow-sb-max)
                  (return-from 2dg--collision 't))))
            b-segments)))
       a-segments)
      'nil)))
(cl-defmethod 2dg-distance ((path 2dg-path) (point 2dg-point))
  "Return the minimum distance between PATH and POINT.

The implementation is not efficient, use caution."
  (cl-loop for segment in (2dg-segments path)
           for distance = (2dg-distance segment point)
           with best = 'nil
           do (when (or (null best) (< distance best))
                (setq best distance))
           finally return best))

(cl-defmethod 2dg-has-intersection ((rect 2dg-rect) (path 2dg-path) &optional evaluation-mode)
  "Return non-nil if RECT intersects PATH using EVALUATION-MODE."
  (cl-loop for segment in (2dg-segments path)
           when (2dg-has-intersection rect segment evaluation-mode)
           return t
           finally return nil))
(cl-defmethod 2dg-simplify ((path 2dg-path))
  "Return a simplified version of the PATH.

Removes duplicates and colinear points."
  (with-slots (points) path
    (2dg-path :points (2dg-simplified points))))
(cl-defmethod 2dg-simplify ((path 2dg-cardinal-path))
  "Return a simplified version of the PATH.

Removes duplicates and colinear points."
  (with-slots (points) path
    (2dg-cardinal-path :points (2dg-simplified points))))

(defun 2dg---path-get-deltas (points)
  "Given a POINTS (a *list* of points), return a list of deltas between the points.

Will return nil if the number of points is 1 or less."
    (if (< (length points) 2)
        'nil
      (let ((last-point (first points))
            (deltas 'nil))
        (mapc (lambda (pt)
                (push (2dg-subtract pt last-point) deltas)
                (setq last-point pt))
              (cdr points))
        (nreverse deltas))))
(defun 2dg---path-from-deltas (deltas start-point)
  "Return a list of points starting at START-POINT and having DELTAS as per point deltas.

DELTAS must be a list of 2dg-points.

Something of the opposite of 2dg---path-get-deltas."
  (let ((points (list start-point)))
    (mapc (lambda (delta)
            (push (2dg-add delta (first points)) points))
          deltas)
    (nreverse points)))

;; Path *building* functions:
(cl-defmethod 2dg-build-path-straight-line ((start 2dg-point) (end 2dg-point))
  "Make a path which is a single straight line from START to END.

This might be an 2dg-path or an 2dg-cardinal-path."
  (condition-case nil
      (2dg-cardinal-path :points `(,start ,end))
    (error (2dg-path :points `(,start ,end)))))
(defun 2dg---path-cardinal-direction (start-pt entry-direction destination-pt)
  "Given a START-PT point coming from ENTRY-DIRECTION find a vector to get closer to DESTINATION-PT.

This is a helper function for the cardinal direction path
generator."
  (let* ((unit-direction (2dg-normalized entry-direction))
         (delta (2dg-subtract destination-pt start-pt))
         (unit-delta (2dg-normalized delta))
         (dot (2dg-dot-prod unit-direction unit-delta)))
    (cond
     ;; must go _backwards_, turn around.
     ((<= dot 2dg--almost-zero)
      ;; go one direction or the other, determine which get closer
      (let ((dir-rot-90 (2dg-rotate-90 unit-direction)))
        (if (>= (2dg-dot-prod dir-rot-90 unit-delta) 0.0)
            ;; guessed direction is correct
            dir-rot-90
          (2dg-additive-inverse dir-rot-90))))
     ;; just keep going?
     ('t
      unit-direction))))
(defun 2dg---path-cardinal (start end entry-vector exit-vector min-segment-distance &optional strict-min-segment-distance)
  "Build a cardinal direction path between START and END.

Function will ensure that the path does not conflict with
ENTRY-VECTOR and EXIT-VECTOR constraints.

When STRICT-MIN-SEGMENT-DISTANCE is non-nil, never create a
segment of length less than MIN-SEGMENT-DISTANCE.  Otherwise,
default to making segments of at least MIN-SEGMENT-DISTANCE but
allow shorter segments in extraorinary cases.

This function works by examining the start point and end point
and trying to draw a path from each point that will intersect
someplace (hopefully in the middle).  If this is not possible it
will determine which (start or end) is the furthest away from
being able to join in the middle and will attach a segment to it
to take a step closer to intersectionthe next time this function
is called.  This function will continue to take steps towards
joining start and end, recursing until it joins them."
  (let ((start-vec (2dg---path-cardinal-direction start
                                                    entry-vector
                                                    end))
        (reverse-end-vec (2dg---path-cardinal-direction end
                                                         (2dg-additive-inverse exit-vector)
                                                         start)))
    (cl-flet ((perpendicular-vectors?
               (A B)
               (>= (abs (2dg-cross-prod A B))
                   2dg--almost-zero))
              (perpendicular-collision
               (A-pt A-dir B-pt)
               ;; this collision can only happen at one of two points.
               ;; (2dg-point :x from A-pt and :y from B-pt)
               ;; (2dg-point :x from B-pt and :y from A-pt)
               (if (equal (2dg-x A-dir) 0.0)
                   ;; A-dir is vertical, therefore your intersection must be at A-pt's X
                   ;; determine if the collision is at A or B's Y coordinates
                   (2dg-point :x (2dg-x A-pt) :y (2dg-y B-pt))
                 ;; A-dir is horizontal, therefore your intersection must be at A-pt's Y
                 (2dg-point :x (2dg-x B-pt) :y (2dg-y A-pt)))))
      (cond
       ;; Vectors are heading in perpendicular directions.  If possible join them
       ;; otherwise turn the best candidate towards a future collision.
       ((perpendicular-vectors? start-vec reverse-end-vec)
        (let* ((intersection-pt (perpendicular-collision start start-vec end))
               (start-delta (2dg-subtract intersection-pt start))
               (start-parametric (2dg-dot-prod start-vec start-delta))
               (end-delta (2dg-subtract intersection-pt end))
               (end-parametric (2dg-dot-prod reverse-end-vec end-delta)))
          ;; -if both are positive, you've found a connection and you're done.
          ;; -if one of them is negative, that vec goes min distance the other
          ;; goes zero distance
          ;; -if both of them are negative they both go min-distance
          (if (> start-parametric 0.0)
              (if (> end-parametric 0.0)
                  ;; both are positive, done.
                  (list start intersection-pt end)

                ;; start is positive, end is negative.
                ;; start goes zero distance, end goes min distance, try again.
                (let ((pre-end (2dg-add end (2dg-scaled reverse-end-vec
                                                            min-segment-distance))))
                  (append (2dg---path-cardinal start
                                               pre-end
                                               entry-vector
                                               (2dg-additive-inverse reverse-end-vec)
                                               min-segment-distance)
                          (list end))))
            ;; start-parametric is =< 0
            (if (> end-parametric 0.0)
            ;; start-parametric is negative(or zero), end is positive.
            ;; end goes zero, start goes min distance, try again
                (let ((post-start (2dg-add start
                                             (2dg-scaled start-vec
                                                           min-segment-distance))))
                  (cons start
                        (2dg---path-cardinal post-start
                                             end
                                             start-vec
                                             exit-vector
                                             min-segment-distance)))
              ;; start parametric is negative, end parametric is negative
              ;; both of them go min distance
              (let ((post-start (2dg-add start
                                           (2dg-scaled start-vec
                                                         min-segment-distance)))
                    (pre-end (2dg-add end
                                        (2dg-scaled reverse-end-vec
                                                      min-segment-distance))))
                (cons start
                      (append (2dg---path-cardinal post-start
                                                     pre-end
                                                     start-vec
                                                     (2dg-additive-inverse reverse-end-vec)
                                                     min-segment-distance)
                              (list end))))))))

       ;; the vectors are parallel and going in opposites directions
       ;; - find a midway point along continuations, go there - path jog
       ((< (2dg-dot-prod start-vec reverse-end-vec) 0.0)
        (let* ((delta (2dg-subtract end start))
               (parallel-distance (2dg-dot-prod delta start-vec))
               (perp-distance (abs (2dg-cross-prod delta start-vec))))

          (cond
           ;; if the perp-distance is < almost-zero then just slap them together
           ((and (< perp-distance 2dg--almost-zero)
                 (>= parallel-distance min-segment-distance))
            ;; satisfies the constraints for a direct connection
            (list start end))

           ;; things can't be solved in one shot
           ((<= parallel-distance 0.0)
            ;; these vectors don't actually get you any closer together.
            ;; looks like these constraints need a few more segments to be able to solve
            ;; bend one of the paths in the correct direction, travel the minimum
            ;; distance then try again.
            (let ((post-start (2dg-add start
                                         (2dg-scaled start-vec min-segment-distance))))
              (cons start
                    (2dg---path-cardinal post-start
                                         end
                                         start-vec
                                         exit-vector
                                         min-segment-distance))))

            ;; if the perp-distance is < min-segment-distance this algorithm won't work.
            ;; if the parallel distance is < 2*min-segment distance this algorithm won't work
            ;;  in either of those two cases this will have to get a bit smarter.
           ((and strict-min-segment-distance
                 (or (< perp-distance min-segment-distance)
                     (< (* 2.0 parallel-distance) min-segment-distance)))

            (error "Unable to find cardinal path right now, make path router smarter[ start %s, end %s, perp-distance %s, parallel-dist %s]"
                   (2dg-pprint start)
                   (2dg-pprint end)
                   perp-distance
                   parallel-distance))


           ;; ok, conditions satisfied for a jog type path.
           ('t
            (let ((midway-displacement (2dg-scaled start-vec (/ parallel-distance 2.0))))
              (list start
                    (2dg-add start midway-displacement)
                    (2dg-subtract end midway-displacement)
                    end))))))

       ;; the vectors are parallel and facing the same direction
       ;; whoever is "ahead" turns, recurse
       ;; use delta to find who is "ahead" - then turn the ahead
       ('t
        (let* ((delta (2dg-subtract end start)))
          (if (> (2dg-dot-prod delta start-vec) 0.0)
              ;;end is further 'ahead'
              (let ((pre-end (2dg-add end
                                        (2dg-scaled reverse-end-vec min-segment-distance))))
                (append (2dg---path-cardinal start
                                             pre-end
                                             entry-vector
                                             (2dg-additive-inverse reverse-end-vec)
                                             min-segment-distance)
                        (list end)))
            ;; start is further 'ahead'
            (let ((post-start (2dg-add start
                                         (2dg-scaled start-vec min-segment-distance))))
              (cons start
                    (2dg---path-cardinal post-start
                                         end
                                         start-vec
                                         exit-vector
                                         min-segment-distance))))))))))
(cl-defgeneric 2dg-build-path-cardinal ((start 2dg-point) (end 2dg-point) (entry-vector 2dg-point) (exit-vector 2dg-point) (min-segment-distance number) &optional min-start-segment-distance min-end-segment-distance)
  "Create a cardinal path joining START and END.

The ENTRY-VECTOR describes the direction the path should start if
possible and the EXIT-VECTOR describes the direction the path
should end if possible.  The only assurance is that the computed
entry and exit directions will not be 180 degrees different from
the EXIT-VECTOR and ENTRY-VECTOR.

MIN-SEGMENT-DISTANCE is a goal parameter.  The algorithm will try
not to make any connecting segments that are shorter than this
parameter.  No promises are made.")
(cl-defmethod  2dg-build-path-cardinal ((start 2dg-point) (end 2dg-point) (entry-vector 2dg-point) (exit-vector 2dg-point) (min-segment-distance number) &optional min-start-segment-distance min-end-segment-distance)
  "Create a cardinal path joining START and END.

The ENTRY-VECTOR describes the direction the path should start if
possible and the EXIT-VECTOR describes the direction the path
should end if possible.  The only assurance is that the computed
entry and exit directions will not be 180 degrees different from
the EXIT-VECTOR and ENTRY-VECTOR.

MIN-SEGMENT-DISTANCE is a goal parameter.  The algorithm will try
not to make any connecting segments that are shorter than this
parameter.  No promises are made.
"
  (let ((original-start nil)
        (original-end nil))
    (when min-start-segment-distance
      (setq original-start start)
      (setq start (2dg-add (2dg-scaled (2dg-normalized entry-vector)
                                       min-start-segment-distance)
                           start)))
    (when min-end-segment-distance
      (setq original-end end)
      (setq end (2dg-add (2dg-scaled (2dg-normalized exit-vector)
                                     (* -1.0 min-end-segment-distance))
                           end)))
    (let ((points (2dg---path-cardinal start end entry-vector exit-vector min-segment-distance)))
      (2dg-cardinal-path :points
                         (if original-start
                             ;; must tack on a start point
                             (if original-end
                                 ;; must tack on a start and end point
                                 (nconc (cons original-start points)
                                        (list original-end))
                               ;; must tack on only the start point
                               (cons original-start points))
                           ;; no start point needed
                           (if original-end
                               ;; must tack on an end point
                               (nconc points
                                      (list original-end))
                             ;; no start point or end point, use points as they are
                             points))))))
(defun 2dg-simplified (&rest n-path-pts)
  "Take all points from N-PATH-PTS lists-of-points and simplify them.

Remove duplicate points.
Remove colinear intermediary points."
  (let ((rev-s-points 'nil)           ;reverse order simplified points
        (last-vec 'nil)
        (last-point 'nil))

    ;; Find the first non-null path and get the first point from it.
    ;; Use that first point to start accumulation.
    (cl-loop for paths-remaining on n-path-pts
             for first-path = (first paths-remaining)
             when first-path
             return (progn
                      (setf last-point (first first-path)
                            n-path-pts paths-remaining)
                      (push last-point rev-s-points)
                      (setcar n-path-pts (cdr first-path))))

    (mapc (lambda (path-pts)
            (mapc (lambda (pt)
                    (when (not (2dg-almost-equal pt last-point))
                      (let ((cur-vec (2dg-normalized (2dg-subtract pt last-point))))
                        (when (and last-vec
                                   (not (2dg-almost-equal cur-vec last-vec)))
                            (push last-point rev-s-points))
                        (setq last-vec cur-vec)))
                    (setq last-point pt))
                  path-pts))
          n-path-pts)
    ;; Push the very last point on to the output.
    (nreverse
     (if (2dg-almost-equal last-point (first rev-s-points))
                 rev-s-points
       (push last-point rev-s-points)))))
(defun 2dg-slack-simplified (path-pts slack-allowance-x slack-allowance-y)
  "Simplify PATH-PTS by allowing changes within a slack allowance.

Allow returned path to change by as much as SLACk-ALLOWANCE-X and
SLACK-ALLOWANCE-Y.

This simplifier will move points around for the purpose of having
less points.  Start and end points will be preserved exactly."
  ;; find any segments smaller than viewport's pixel size.
  ;;    so this won't work. I have to do these comparisons absolutely.
  (let ((last-path-pt)
        (num-compacted-pts 1)
        (num-pts 0)
        (last-compacted-pt (first path-pts))
        (compacted-pts (list (first path-pts))))
    (cl-loop for real-pt in (cdr path-pts)
             for delta = (2dg-subtract real-pt last-compacted-pt)
             for abs-delta-x = (abs (2dg-x delta))
             for abs-delta-y = (abs (2dg-y delta))
             ;; if you went through either slack add a new point with the slack
             ;; that went over.
             do (cond ((>= abs-delta-x slack-allowance-x)
                       ;; X went over allowance.
                       (if (>= abs-delta-y slack-allowance-y)
                           ;; X and Y went over allowance.
                           (push real-pt compacted-pts)
                         ;; X went over but Y did not.
                         (push (2dg-point :x (2dg-x real-pt) :y (2dg-y last-compacted-pt))
                               compacted-pts))
                       (incf num-compacted-pts)
                       (setq last-compacted-pt (first compacted-pts)))
                      ((>= abs-delta-y slack-allowance-y)
                       ;; Y went over but X did not
                       (push (2dg-point :x (2dg-x last-compacted-pt) :y (2dg-y real-pt))
                             compacted-pts)
                       (setq last-compacted-pt (first compacted-pts))
                       (incf num-compacted-pts)))
             do (setq last-path-pt real-pt
                      num-pts (1+ num-pts)))
    (if (2dg-almost-equal last-path-pt last-compacted-pt)
        ;; end point matches, no additional work needed.
        (nreverse compacted-pts)
      (cond ((let ((remaining-delta (2dg-subtract last-compacted-pt last-path-pt)))
               (and (2dg-cardinal-direction-vector-p remaining-delta)
                    (< (2dg-x remaining-delta) slack-allowance-x)
                    (< (2dg-y remaining-delta) slack-allowance-y)))
             ;; The last point you compacted is one single cardinal
             ;; segment away from the true path end.  Additionally it's
             ;; sufficiently close.  Simply push the path end on and
             ;; complete.
             (nreverse (cons last-path-pt compacted-pts)))
            ((>= num-compacted-pts 3)
             ;; End point does not match, amend the first N points to handle this.
             ;; note: if the list is only 2 elements long this will be impossible and
             ;;       the path router needs to be called.
             (let* ((required-delta (2dg-subtract last-path-pt last-compacted-pt))
                    (required-unit-vec (2dg-normalized required-delta))
                    (failure))
               ;; go through compacted-pts, adding this delta until you hit a segment
               ;; wich you don't need to.
               ;; TODO - this only takes direction into account, it could produce paths
               ;;        that overlap themselves.  - handle that.
               (cl-loop for reverse-path on compacted-pts
                        for start-pt = (first reverse-path)
                        for end-pt = (second reverse-path)
                        ;; if you got to the end point and you're still trying
                        ;; to correct things then this algorithm failed.
                        unless (and end-pt)
                        do (progn (setq failure t)
                                  (cl-return))
                        for segment-char-vec = (2dg-subtract end-pt start-pt)
                        for segment-unit-vec = (2dg-normalized segment-char-vec)

                        ;; the start point *must* be moved.
                        do (2dg-incf start-pt required-delta)

                        ;; if the dot product is zero, then you have to move this point, it won't have freedom
                        ;; in the required direction.
                        do (unless (2dg-almost-zero (2dg-dot-prod required-unit-vec segment-unit-vec))
                             (cl-return)))
               (2dg-simplified (if failure
                                   path-pts
                                 (nreverse compacted-pts)))))
            (t
             ;; There are 2 points or less in the path.  If this can be
             ;; completed With a single path segment then that's ok,
             ;; Otherwise, fail.
             ;; Only 2 points or less in the path, and no cardinal
             ;; displacement segment is a valid completion. Just give up
             ;; for now.
             path-pts)))))

(cl-defgeneric 2dg-stretch (path (force-start 2dg-point) (force-end 2dg--point))
  "Return a new path based off PATH stretched to match FORCE-START and FORCE-END.")
(cl-defmethod 2dg-stretch ((points list) (force-start 2dg-point) (force-end 2dg-point))
  "Return a new list of points based off POINTS being stretched to FORCE-START and FORCE-END.

"
  (2dg---path-stretch points force-start force-end))
(defun 2dg---path-stretch (points force-start force-end)
  "Return a path (list) from FORCE-START to FORCE-END which 'feels' like POINTS.

The typical use case is when there was formerly a path
characterized by POINTS which connected two things.  For any
reason one or both of those two things which the path connected
were moved slightly.  This function will return a list of points
which tries to look like the original points but is stretched to
have the desired start and end points."
  (let* ((old-start (first points))
         (old-end (car (last points)))
         (force-displacement (2dg-subtract force-end force-start))
         (old-displacement (2dg-subtract old-end old-start)))
    (when (and (2dg-almost-equal old-start force-start)
               (2dg-almost-equal old-end force-end))
      ;; TODO - deterimine the source of this and attempt to remove.
      ;; (error "Call to path-stretch but no stretching was required?")
      points)

    (cl-flet ((get-scale (force-displacement old-displacement)
                         (if (equal old-displacement 0.0)
                             (if (equal old-displacement force-displacement)
                                 1.0    ;they're the same, no scale needed.
                               nil)    ;they're not the same and the old displacement is zero - no applicable scaling
                           (/ force-displacement old-displacement)))
              (path-stretch-freedom (points)
                                    (cl-loop with x-free = 'nil
                                             with y-free = 'nil
                                             with last-pt = (first points)
                                             for pt in (cdr points)
                                             for delta = (2dg-subtract pt last-pt)
                                             do (setf x-free (or x-free (not (equal (2dg-x delta) 0.0)))
                                                      y-free (or y-free (not (equal (2dg-y delta) 0.0))))
                                             when (and x-free y-free)
                                             return 't
                                             do (setq last-pt pt)
                                             finally return 'nil)))

      (let ((scale-x (get-scale (2dg-x force-displacement) (2dg-x old-displacement)))
            (scale-y (get-scale (2dg-y force-displacement) (2dg-y old-displacement))))
        (if (and scale-x scale-y)
            ;; both scalings are valid, apply them and finish
            (let ((scale (2dg-point :x scale-x :y scale-y)))
              (mapcar (lambda (point)
                        (2dg-add (2dg-scaled (2dg-subtract point old-start) scale)
                                   force-start))
                      points))
          ;; one scaling or the other is not applicable, must jog or offset things.
          (if (not (path-stretch-freedom points))
              ;; there is no freedom in this path, just insert a jog.
              (2dg---path-cardinal force-start
                                     force-end
                                     (2dg-subtract (second points)
                                                     (first points))
                                     (2dg-subtract (car (last points))
                                                     (car (last points 2)))
                                     1.0) ;TODO - this 1.0 is a guess, I'm not sure what it should really be
            ;; You have the liberty to inject displacements into this path to
            ;; make it stretch correctly.  Determine the displacement.
            (let ((deltas (2dg---path-get-deltas points))
                  (additional-displacement (2dg-subtract force-displacement old-displacement))
                  (vertical-deltas 'nil)
                  (horizontal-deltas 'nil))
              (cl-loop for delta in deltas
                       do (when (not (equal (2dg-x delta) 0.0))
                            (push delta horizontal-deltas))
                       do (when (not (equal (2dg-y delta) 0.0))
                            (push delta vertical-deltas)))
              ;; modify deltas
              (unless (equal (2dg-x additional-displacement) 0.0)
                (let ((additional-horizontal (/ (2dg-x additional-displacement)
                                                (float (length horizontal-deltas)))))
                  (cl-loop for delta in horizontal-deltas
                           do (oset delta x (+ (2dg-x delta) additional-horizontal)))))
              (unless (equal (2dg-y additional-displacement) 0.0)
                (let ((additional-vertical (/ (2dg-y additional-displacement)
                                              (float (length vertical-deltas)))))
                  (cl-loop for delta in vertical-deltas
                           do (oset delta y (+ (2dg-y delta) additional-vertical)))))
                (2dg---path-from-deltas deltas force-start))))))))

(defun 2dg---nudge-path-start (points move-vector)
  "Move the first element of POINTS by MOVE-VECTOR and update subsequent points."
  (unless points
    (error "Must provide at least one point for nudge-path-start"))
  (let ((num-points (length points)))
    (if (= num-points 1)
        ;; if there's only one point, just move it.
        (list (2dg-add (car points) move-vector))
      ;; there is more than one point.
      (let* ((first-point (first points))
             (raw-offset (2dg-subtract (second points) first-point))
             (raw-box-magnitude (2dg-box-magnitude raw-offset))
             (first-offset (if (2dg-almost-zero raw-box-magnitude)
                               ;; If there's no displacement between these two points
                               ;; then they're likely in the exact same place.
                               ;; If true I can separate them in any direction I choose
                               ;; so I choose move-vector
                               move-vector
                             ;; normal path - these two points are not the same.
                             raw-offset))
             (first-unit-offset (2dg-normalized first-offset))
             (parallel-move (2dg-scaled first-unit-offset
                                          (2dg-dot-prod first-unit-offset move-vector)))
             (perpendicular-move (2dg-subtract move-vector parallel-move)))

        (if (2dg-almost-zero (2dg-box-magnitude perpendicular-move))
            ;; You're requesting a move in a direction parallel to your
            ;; current segment direction, just allow the movement.
            (cons (2dg-add first-point move-vector)
                  (cdr points))
          ;; You're moving in a direction _not_entirely_ parallel
          ;; move your first point by move-vec and append to
          ;; recursion with (cdr path) and perpendicular-move
          (cons (2dg-add first-point move-vector)
                (2dg---nudge-path-start (cdr points) perpendicular-move)))))))
(defun 2dg--nudge-path (path-points point-idx move-vector)
  "Path nudger."
  (when (or (< point-idx 0) (>= point-idx (length path-points)))
    (error "Error: point-idx must be < path-points length"))
  (let ((before-path (list (first path-points)))
        (after-path path-points))
    (dotimes (idx point-idx)
      (setq after-path (cdr after-path))
      (setq before-path (cons (first after-path) before-path)))
    (append (reverse (2dg---nudge-path-start before-path move-vector))
            (cdr (2dg---nudge-path-start after-path move-vector)))))

(cl-defgeneric 2dg-nudge-path (path (point-idx integer) (move-vector 2dg-point))
  "Nudge POINT-IDX point in PATH by MOVE-VECTOR and update surrounting points.

Returns an updated path.")
(cl-defmethod 2dg-nudge-path ((path 2dg-path) (point-idx integer) (move-vector 2dg-point))
    "Nudge POINT-IDX point in PATH by MOVE-VECTOR and update surrounding points.

There are no promises made that other points (other than at
POINT-IDX) will stay static.  Specifically the starting and
ending points may change even though a mid point was indicated by
POINT-IDX.")
(cl-defmethod 2dg-nudge-path ((path list) (point-idx integer) (move-vector 2dg-point))
  "Nudge POINT-IDX point in PATH by MOVE-VECTOR and update surrounding points.

There are no promises made that other points (other than at
POINT-IDX) will stay static.  Specifically the starting and
ending points may change even though a mid point was indicated by
POINT-IDX."
  (2dg--nudge-path path point-idx move-vector))
  ;; (when (or (< point-idx 0) (>= point-idx (length path)))
  ;;   (error "Error: point-idx must be < path length"))
  ;; (let ((before-path (list (first path)))
  ;;       (after-path path))
  ;;   (dotimes (idx point-idx)
  ;;     (setq after-path (cdr after-path))
  ;;     (setq before-path (cons (first after-path) before-path)))
  ;;   (append (reverse (2dg---nudge-path-start before-path move-vector))
  ;;           (cdr (2dg---nudge-path-start after-path move-vector)))))

(provide '2dg-path)
;;; 2dg-path.el ends here
