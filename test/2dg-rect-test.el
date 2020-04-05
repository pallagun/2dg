(require 'ert)

(require '2dg-rect)

(ert-deftest 2dg-rect-segments-matches ()
  (let* ((rect (2dg-rect :x-min 0.0 :x-max 5.0
                         :y-min 2.0 :y-max 7.0))
         (segments (2dg-segments rect)))
    (should (2dg-almost-equal (first segments)
                              (2dg-bottom rect)))
    (should (2dg-almost-equal (second segments)
                              (2dg-right rect)))
    (should (2dg-almost-equal (third segments)
                              (2dg-top rect)))
    (should (2dg-almost-equal (fourth segments)
                              (2dg-left rect)))))

(ert-deftest 2dg-rect-contains-rect-point ()
  (let ((rect (2dg-rect :x-min 44.0 :x-max 45.0
                        :y-min 19.0 :y-max 20.0))
        (test-LL (2dg-point :x 44.0 :y 19.0))
        (test-LR (2dg-point :x 45.0 :y 19.0))
        (test-TR (2dg-point :x 45.0 :y 20.0))
        (test-TL (2dg-point :x 44.0 :y 20.0)))
    (should (2dg-contains rect test-LL))
    (should (2dg-contains rect test-LL nil))
    (should (2dg-contains rect test-LL 'stacked))
    (should-not (2dg-contains rect test-LL 'strict))

    (should (2dg-contains rect test-LR))
    (should (2dg-contains rect test-LR nil))
    (should-not (2dg-contains rect test-LR 'stacked))
    (should-not (2dg-contains rect test-LR 'strict))

    (should (2dg-contains rect test-TR))
    (should (2dg-contains rect test-TR nil))
    (should-not (2dg-contains rect test-TR 'stacked))
    (should-not (2dg-contains rect test-TR 'strict))

    (should (2dg-contains rect test-TL))
    (should (2dg-contains rect test-TL nil))
    (should-not (2dg-contains rect test-TL 'stacked))
    (should-not (2dg-contains rect test-TL 'strict))))

(ert-deftest 2dg-rect-has-intersection-segment ()

  (cl-flet ((append-flipped
             (segment-list)
             (cl-loop with accumulator = nil
                      for segment in segment-list
                      do (push segment accumulator)
                      do (push (2dg-flipped segment) accumulator)
                      finally return accumulator)))
    ;; strange test cases
    (let ((rect #s(2dg-rect 0.0 0.0 1.0 1.0))
          (segment #s(2dg-segment #s(2dg-point 1.0 2.0) #s(2dg-point 1.0 -1.0))))
      (should-not (2dg-has-intersection rect segment 'stacked)))

    ;; (let ((rect #s(2dg-rect 51.0 13.0 14.0 52.0))
    ;;       (segment #s(2dg-segment #s(2dg-point 52.0 19.0) #s(2dg-point 52.0 6.0))))
    ;;   (should-not (2dg-has-intersection rect segment 'stacked)))

    (let ((rect (2dg-rect- 0 1 0 1)))
      ;; This segment is nowhere near, so there should be no intersection
      (let ((segment (2dg-segment- 2 3 4 5)))
        (should-not (2dg-has-intersection rect segment))
        (should-not (2dg-has-intersection rect segment nil))
        (should-not (2dg-has-intersection rect segment 'stacked))
        (should-not (2dg-has-intersection rect segment 'strict)))
      ;; This segment is nowhere near but it spans multiple voronoi regions
      (let ((segment (2dg-segment- 0.5 -3 3 0.5)))
        (should-not (2dg-has-intersection rect segment))
        (should-not (2dg-has-intersection rect segment nil))
        (should-not (2dg-has-intersection rect segment 'stacked))
        (should-not (2dg-has-intersection rect segment 'strict)))
      ;; This segmens on the far right - should not pass strict or stacked.
      (let ((segments (append-flipped (list (2dg-segment- 1 0.3 1 0.7)
                                            (2dg-segment- 1 0 1 1)
                                            (2dg-segment- 1 -1 1 2)))))
        (cl-loop for segment in segments
                 do (progn
                      (should (2dg-has-intersection rect segment))
                      (should (2dg-has-intersection rect segment nil))
                      (should-not (2dg-has-intersection rect segment 'stacked))
                      (should-not (2dg-has-intersection rect segment 'strict)))))
      ;; This segment is on the top so it should not pass strict or stacked.
      (let ((segment (2dg-segment- 0.3 1 1.3 1)))
        (should (2dg-has-intersection rect segment))
        (should (2dg-has-intersection rect segment nil))
        (should-not (2dg-has-intersection rect segment 'stacked))
        (should-not (2dg-has-intersection rect segment 'strict)))
      ;; This segment is on the left so it should not pass strict
      (let ((segment (2dg-segment- 0 0.3 0 0.7)))
        (should (2dg-has-intersection rect segment))
        (should (2dg-has-intersection rect segment nil))
        (should (2dg-has-intersection rect segment 'stacked))
        (should-not (2dg-has-intersection rect segment 'strict))))))


(provide '2dg-rect-test)
