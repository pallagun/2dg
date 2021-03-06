(require 'ert)

(require '2dg-segment)

(ert-deftest 2dg-segment-distance-parallel-segment-segment ()
  "test distance between two segments given they are parallel"
  (let ((base-segment (2dg-segment :start (2dg-point- 0 0)
                                     :end (2dg-point- 10 0)))
        (diag-segment (2dg-segment :start (2dg-point- -10 -10)
                                     :end (2dg-point- 0 0))))
    (cl-flet ((should-all-combos
               (A B distance)
               (let ((A-flip (2dg-flipped A))
                     (B-flip (2dg-flipped B)))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel A B)
                          distance))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel A B-flip)
                          distance))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel B A)
                          distance))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel B A-flip)
                          distance))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel A-flip B-flip)
                          distance))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel A-flip B)
                          distance))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel B-flip A-flip)
                          distance))
                 (should (2dg-almost-equal
                          (2dg---distance-parallel B-flip A)
                          distance)))))
      ;; parallel, 100% overlap for both
      (should-all-combos base-segment
                         (2dg-segment :start (2dg-point- 0 2)
                                        :end (2dg-point- 10 2))
                         2.0)
      ;; parallel 50% overlap for both
      (should-all-combos base-segment
                         (2dg-segment :start (2dg-point- 5 2)
                                        :end (2dg-point- 15 2))
                         2.0)
      ;; parallel 50% overlap for one
      (should-all-combos base-segment
                         (2dg-segment :start (2dg-point- 0 2)
                                        :end (2dg-point- 15 2))
                         2.0)
      ;; parallel 100% overlap for one, 50% overlap for another, totally encompassing
      (should-all-combos base-segment
                         (2dg-segment :start (2dg-point- 3 2)
                                        :end (2dg-point- 7 2))
                         2.0)
      ;; parallel no overlap A+, B-
      (should-all-combos base-segment
                         (2dg-segment :start (2dg-point- 11 1)
                                        :end (2dg-point- 20 1))
                         (sqrt 2.0))
      ;; diagonal full overlap
      (should-all-combos diag-segment
                         (2dg-segment :start (2dg-point- -11 -9)
                                        :end (2dg-point- -1 1))
                         (sqrt 2.0))
      ;; diagonal partial overlap
      (should-all-combos diag-segment
                         (2dg-segment :start (2dg-point- -6 -4)
                                        :end (2dg-point- -4 -2))
                         (sqrt 2.0))
      ;; diagonal end voronoi region
      (should-all-combos diag-segment
                         (2dg-segment :start (2dg-point- -11 -11)
                                        :end (2dg-point- -20 -20))
                         (sqrt 2.0))
      ;; parallel and colinear - no overlap
      (should-all-combos base-segment
                         (2dg-segment :start (2dg-point- 11 0)
                                        :end (2dg-point- 20 0))
                         1.0)
      ;; parallel and colinear - with overlap
      (should-all-combos base-segment
                         (2dg-segment :start (2dg-point- 8 0)
                                        :end (2dg-point- 18 0))
                         0.0)
      )))

(ert-deftest 2dg-segment-distance-segment-segment ()
  "tests for segment to segment distance where they are not parallel"
  (cl-flet ((should-all-combos
             (A B distance)
             (let ((A-flip (2dg-flipped A))
                   (B-flip (2dg-flipped B)))
               (should (2dg-almost-equal
                        (2dg-distance A B)
                        distance))
               (should (2dg-almost-equal
                        (2dg-distance A B-flip)
                        distance))
               (should (2dg-almost-equal
                        (2dg-distance B A)
                        distance))
               (should (2dg-almost-equal
                        (2dg-distance B A-flip)
                        distance))
               (should (2dg-almost-equal
                        (2dg-distance A-flip B-flip)
                        distance))
               (should (2dg-almost-equal
                        (2dg-distance A-flip B)
                        distance))
               (should (2dg-almost-equal
                        (2dg-distance B-flip A-flip)
                        distance))
               (should (2dg-almost-equal
                        (2dg-distance B-flip A)
                        distance)))))
    ;; lines that cross, dead center
    (should-all-combos (2dg-segment :start (2dg-point :x 0.0 :y 0.0)
                                      :end (2dg-point :x 5.0 :y 5.0))
                       (2dg-segment :start (2dg-point :x 5.0 :y 0.0)
                                      :end (2dg-point :x 0.0 :y 5.0))
                       0.0)
    ;; lines cross, not dead center
    (should-all-combos (2dg-segment :start (2dg-point :x 0.0 :y 0.0)
                                      :end (2dg-point :x 10.0 :y 0.0))
                       (2dg-segment :start (2dg-point :x 8.0 :y -1.0)
                                      :end (2dg-point :x 6.0 :y 6.0))
                       0.0)
    ;; lines touch, end to segment
    (should-all-combos (2dg-segment :start (2dg-point :x 0.0 :y 0.0)
                                      :end (2dg-point :x 10.0 :y 0.0))
                       (2dg-segment :start (2dg-point :x 0.0 :y 2.0)
                                      :end (2dg-point :x 2.0 :y 0.0))
                       0.0)
    ;; lines don't touch - voronoi region of end points only
    (should-all-combos (2dg-segment :start (2dg-point :x 0.0 :y 0.0)
                                      :end (2dg-point :x 5.0 :y 5.0))
                       (2dg-segment :start (2dg-point :x 6.0 :y 6.0)
                                      :end (2dg-point :x 6.0 :y 22.0))
                       (sqrt 2.0))
    ;; lines don't touch - voronoi region of segment to an end point
    (should-all-combos (2dg-segment :start (2dg-point :x 0.0 :y 0.0)
                                      :end (2dg-point :x 10.0 :y 10.0))
                       (2dg-segment :start (2dg-point :x 4.0 :y 8.0)
                                      :end (2dg-point :x 4.0 :y 6.0))
                       (sqrt 2.0))
    ))

(ert-deftest 2dg-segment-distance-segment-point ()
  "tests for segment to point distance"
  (let ((horzontal-segment (2dg-segment :start (2dg-point :x 0.0 :y 0.0)
                                          :end (2dg-point :x 10.0 :y 0.0)))
        (diagonal-segment (2dg-segment :start (2dg-point :x 0.0 :y 0.0)
                                         :end (2dg-point :x -10.0 :y -10.0))))
    ;; within segment voronoi region
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x 1.0 :y 1.0))
             1.0))
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x 2.0 :y -4.0))
             4.0))
    ;; on cusp of voronoi regions
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x 10.0 :y 100.0))
             100.0))
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x 0.0 :y -202.0))
             202.0))
    ;; start point voronoi region
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x -2.0 :y 0.0))
             2.0))
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x -1.0 :y -1.0))
             (sqrt 2.0)))
    ;; end point voronoi region
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x 1000.0 :y 0.0))
             990.0))
    (should (2dg-almost-equal
             (2dg-distance horzontal-segment (2dg-point :x 20.0 :y 10.0))
             (* 10.0 (sqrt 2.0))))
    ;; now some stuff with the diagonal segment.
    (should (2dg-almost-equal
             (2dg-distance diagonal-segment (2dg-point :x 1.0 :y 1.0))
             (sqrt 2.0)))
    (should (2dg-almost-equal
             (2dg-distance diagonal-segment (2dg-point :x -10.0 :y -11.0))
             1.0))
    (should (2dg-almost-equal
             (2dg-distance diagonal-segment (2dg-point :x -1.0 :y 0.0))
             (/ (sqrt 2.0) 2.0)))

    (should (2dg-almost-equal
             (2dg-distance diagonal-segment (2dg-point :x -9.0 :y -10.0))
             (/ (sqrt 2.0) 2.0)))))

(ert-deftest 2dg-segment-pierced? ()
  (let ((test-segment (2dg-segment :start (2dg-point- 1 1)
                                     :end (2dg-point- 2 1))))

    ;; lines that are not parallel
    ;; no collision at all
    (let ((piercer (2dg-segment :start (2dg-point- 0 0.5)
                                  :end (2dg-point- 0 1.5))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil t nil t))
      (should-not (2dg-pierced-p piercer test-segment nil t t nil))
      (should-not (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment t nil nil t))
      (should-not (2dg-pierced-p piercer test-segment t nil t nil))
      (should-not (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should-not (2dg-pierced-p piercer test-segment t t nil t))
      (should-not (2dg-pierced-p piercer test-segment t t t nil))
      (should-not (2dg-pierced-p piercer test-segment t t t t)))
    ;; midpoint to midpoint
    (let ((piercer (2dg-segment :start (2dg-point- 1.5 0.5)
                                  :end (2dg-point- 1.0 1.5))))
      (should (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should (2dg-pierced-p piercer test-segment nil nil nil t))
      (should (2dg-pierced-p piercer test-segment nil nil t nil))
      (should (2dg-pierced-p piercer test-segment nil nil t t))
      (should (2dg-pierced-p piercer test-segment nil t nil nil))
      (should (2dg-pierced-p piercer test-segment nil t nil t))
      (should (2dg-pierced-p piercer test-segment nil t t nil))
      (should (2dg-pierced-p piercer test-segment nil t t t))
      (should (2dg-pierced-p piercer test-segment t nil nil nil))
      (should (2dg-pierced-p piercer test-segment t nil nil t))
      (should (2dg-pierced-p piercer test-segment t nil t nil))
      (should (2dg-pierced-p piercer test-segment t nil t t))
      (should (2dg-pierced-p piercer test-segment t t nil nil))
      (should (2dg-pierced-p piercer test-segment t t nil t))
      (should (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's midpoint to arg1's start point.
    (let ((piercer (2dg-segment :start (2dg-point- 1 0.5)
                                  :end (2dg-point- 1 1.5))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should (2dg-pierced-p piercer test-segment nil nil t nil))
      (should (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil t nil t))
      (should (2dg-pierced-p piercer test-segment nil t t nil))
      (should (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment t nil nil t))
      (should (2dg-pierced-p piercer test-segment t nil t nil))
      (should (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should-not (2dg-pierced-p piercer test-segment t t nil t))
      (should (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's midpoint to arg1's end point
    (let ((piercer (2dg-segment :start (2dg-point- 2 0.5)
                                  :end (2dg-point- 2 1.5))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should (2dg-pierced-p piercer test-segment nil t nil t))
      (should-not (2dg-pierced-p piercer test-segment nil t t nil))
      (should (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should (2dg-pierced-p piercer test-segment t nil nil t))
      (should-not (2dg-pierced-p piercer test-segment t nil t nil))
      (should (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should (2dg-pierced-p piercer test-segment t t nil t))
      (should-not (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's start point to arg1's midpoint
    (let ((piercer (2dg-segment :start (2dg-point- 1.5 1)
                                  :end (2dg-point- 1.5 2))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil t nil t))
      (should-not (2dg-pierced-p piercer test-segment nil t t nil))
      (should-not (2dg-pierced-p piercer test-segment nil t t t))
      (should (2dg-pierced-p piercer test-segment t nil nil nil))
      (should (2dg-pierced-p piercer test-segment t nil nil t))
      (should (2dg-pierced-p piercer test-segment t nil t nil))
      (should (2dg-pierced-p piercer test-segment t nil t t))
      (should (2dg-pierced-p piercer test-segment t t nil nil))
      (should (2dg-pierced-p piercer test-segment t t nil t))
      (should (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's end point to arg1's midpoint
    (let ((piercer (2dg-segment :start (2dg-point- 1.5 2)
                                  :end (2dg-point- 1.5 1))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should (2dg-pierced-p piercer test-segment nil t nil nil))
      (should (2dg-pierced-p piercer test-segment nil t nil t))
      (should (2dg-pierced-p piercer test-segment nil t t nil))
      (should (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment t nil nil t))
      (should-not (2dg-pierced-p piercer test-segment t nil t nil))
      (should-not (2dg-pierced-p piercer test-segment t nil t t))
      (should (2dg-pierced-p piercer test-segment t t nil nil))
      (should (2dg-pierced-p piercer test-segment t t nil t))
      (should (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's start point to arg1's start point
    (let ((piercer (2dg-segment :start (2dg-point- 1 1)
                                  :end (2dg-point- 0.5 1.5))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should-not(2dg-pierced-p piercer test-segment nil t nil t))
      (should-not (2dg-pierced-p piercer test-segment nil t t nil))
      (should-not (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment t nil nil t))
      (should (2dg-pierced-p piercer test-segment t nil t nil))
      (should (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should-not (2dg-pierced-p piercer test-segment t t nil t))
      (should (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's start point to arg1's end point
    (let ((piercer (2dg-segment :start (2dg-point- 2 1)
                                  :end (2dg-point- 3 2))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil t nil t))
      (should-not (2dg-pierced-p piercer test-segment nil t t nil))
      (should-not (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should (2dg-pierced-p piercer test-segment t nil nil t))
      (should-not (2dg-pierced-p piercer test-segment t nil t nil))
      (should (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should (2dg-pierced-p piercer test-segment t t nil t))
      (should-not (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's end point to arg1's start point
    (let ((piercer (2dg-segment :start (2dg-point- 1 2)
                                  :end (2dg-point- 1 1))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil t nil t))
      (should (2dg-pierced-p piercer test-segment nil t t nil))
      (should (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment t nil nil t))
      (should-not (2dg-pierced-p piercer test-segment t nil t nil))
      (should-not (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should-not (2dg-pierced-p piercer test-segment t t nil t))
      (should (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; arg0's end point to arg1's end point
    (let ((piercer (2dg-segment :start (2dg-point- 3 2)
                                  :end (2dg-point- 2 1))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should (2dg-pierced-p piercer test-segment nil t nil t))
      (should-not (2dg-pierced-p piercer test-segment nil t t nil))
      (should (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment t nil nil t))
      (should-not (2dg-pierced-p piercer test-segment t nil t nil))
      (should-not (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should (2dg-pierced-p piercer test-segment t t nil t))
      (should-not (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ;; parallel lines?
    ;; no collision at all
    (let ((piercer (2dg-segment :start (2dg-point- 0.5 2)
                                  :end (2dg-point- 1.5 2))))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil nil t))
      (should-not (2dg-pierced-p piercer test-segment nil nil t nil))
      (should-not (2dg-pierced-p piercer test-segment nil nil t t))
      (should-not (2dg-pierced-p piercer test-segment nil t nil nil))
      (should-not (2dg-pierced-p piercer test-segment nil t nil t))
      (should-not (2dg-pierced-p piercer test-segment nil t t nil))
      (should-not (2dg-pierced-p piercer test-segment nil t t t))
      (should-not (2dg-pierced-p piercer test-segment t nil nil nil))
      (should-not (2dg-pierced-p piercer test-segment t nil nil t))
      (should-not (2dg-pierced-p piercer test-segment t nil t nil))
      (should-not (2dg-pierced-p piercer test-segment t nil t t))
      (should-not (2dg-pierced-p piercer test-segment t t nil nil))
      (should-not (2dg-pierced-p piercer test-segment t t nil t))
      (should-not (2dg-pierced-p piercer test-segment t t t nil))
      (should-not (2dg-pierced-p piercer test-segment t t t t)))
    ;; lines with a large section of overlap
    (let ((piercer (2dg-segment :start (2dg-point- 0.5 1)
                                  :end (2dg-point- 1.5 1))))
      (should (2dg-pierced-p piercer test-segment nil nil nil nil))
      (should (2dg-pierced-p piercer test-segment nil nil nil t))
      (should (2dg-pierced-p piercer test-segment nil nil t nil))
      (should (2dg-pierced-p piercer test-segment nil nil t t))
      (should (2dg-pierced-p piercer test-segment nil t nil nil))
      (should (2dg-pierced-p piercer test-segment nil t nil t))
      (should (2dg-pierced-p piercer test-segment nil t t nil))
      (should (2dg-pierced-p piercer test-segment nil t t t))
      (should (2dg-pierced-p piercer test-segment t nil nil nil))
      (should (2dg-pierced-p piercer test-segment t nil nil t))
      (should (2dg-pierced-p piercer test-segment t nil t nil))
      (should (2dg-pierced-p piercer test-segment t nil t t))
      (should (2dg-pierced-p piercer test-segment t t nil nil))
      (should (2dg-pierced-p piercer test-segment t t nil t))
      (should (2dg-pierced-p piercer test-segment t t t nil))
      (should (2dg-pierced-p piercer test-segment t t t t)))
    ))

(ert-deftest 2dg-segment-absolute-coordinates-test ()
  "Absolute coordinates test for segments."
  (let ((segment (2dg-segment- 0 0 1 1)))
    (should (2dg-almost-equal segment
                              (2dg-absolute-coordinates segment
                                                        (2dg-span- 0 1))))
    (should (2dg-almost-equal (2dg-flipped segment)
                             (2dg-absolute-coordinates segment
                                                       (2dg-span- 1 0))))
    (should (2dg-almost-equal (2dg-segment- 0.5 0.5 0.75 0.75)
                              (2dg-absolute-coordinates segment
                                                        (2dg-span- 0.5 0.75))))
    (should (2dg-almost-equal (2dg-segment- 0.75 0.75 0.5 0.5)
                              (2dg-absolute-coordinates segment
                                                        (2dg-span- 0.75 0.5))))
    (should (2dg-almost-equal (2dg-segment- 1 1 2 2)
                              (2dg-absolute-coordinates segment
                                                        (2dg-span- 1 2))))
    (should (2dg-almost-equal (2dg-segment- -1 -1 -2 -2)
                              (2dg-absolute-coordinates segment
                                                        (2dg-span- -1 -2))))))


(provide '2dg-segment-test)
