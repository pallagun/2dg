(require 'ert)
(require '2dg-path)

(defun pts-almost-equal (A B)
  (2dg-almost-equal
   (2dg-path :points A)
   (2dg-path :points B)))

(ert-deftest 2dg-path-is-cardinal-path? ()
  ;; Needs to say that a path with a single point is indeed cardinal
  (let ((paths (list
                (2dg-path :points (list (2dg-point- 0 20)))
                (2dg-path :points (list (2dg-point- 0 0)
                                          (2dg-point- 1 0)))
                (2dg-path :points (list (2dg-point- 0 0)
                                          (2dg-point- 1 0)
                                          (2dg-point- 1 1)))
                (2dg-path :points (list (2dg-point- 30.5 22.0)
                                          (2dg-point- 30.500000000001 17.0))))))
    (mapcar (lambda (path)
              (should (2dg-is-cardinal-pts-list-p (2dg-points path))))
            paths)))

(ert-deftest 2dg-path-cardinal ()
  (let ((right-vec (2dg-point- 1 0))
        (up-vec (2dg-point- 0 1))
        (down-vec (2dg-point- 0 -1))
        (left-vec (2dg-point- -1 0))
        (min-dist 1.0))
    (cl-flet ((should-be-almost-equal
               (A B)
               (should
                (2dg-almost-equal (2dg-path :points A) (2dg-path :points B)))))

      ;; another weird one I ran into.
      (let ((start (2dg-point- 38.5 32))
            (end (2dg-point- 38.5 21))
            (entry-vector (2dg-point- 0 1))
            (exit-vector (2dg-point- 0 -1))
            (min-segment-distance 1.0))
        (let ((computed (2dg---path-cardinal start end entry-vector exit-vector min-segment-distance)))
          (should (eq 5 (length computed)))))

      ;; weird one I ran into.
      (let* ((start (2dg-point- 2.25 36.75))
             (end (2dg-point- 13.0 37.0))
             (entry-vector (2dg-point- 1.0 -0.0))
             (exit-vector (2dg-point- -1.0 0.0))
             (min-segment-distance 1.0)
             (expected-path (list start
                                  (2dg-point- 13.0 36.75)
                                  end)))
        (should (eq 3 (length (2dg---path-cardinal start end entry-vector exit-vector min-segment-distance))))
        (should-be-almost-equal (2dg---path-cardinal start end entry-vector exit-vector min-segment-distance)
                                expected-path))
      ;; simple 90 degree
      (let ((start (2dg-point- 0 0))
            (end (2dg-point- 5 5)))
        (let ((bottom-right-path (list start (2dg-point- 5 0) end)))
          (should-be-almost-equal (2dg---path-cardinal start end
                                                       right-vec up-vec
                                                       min-dist)
                                  bottom-right-path)
          (should-be-almost-equal (2dg---path-cardinal start end
                                                       down-vec up-vec
                                                       min-dist)
                                  bottom-right-path)
          (should-be-almost-equal (2dg---path-cardinal start end
                                                       right-vec left-vec
                                                       min-dist)
                                  bottom-right-path))
        (let ((top-left-path (list start (2dg-point- 0 5) end)))
          (should-be-almost-equal (2dg---path-cardinal start end
                                                       up-vec right-vec
                                                       min-dist)
                                  top-left-path)
          (should-be-almost-equal (2dg---path-cardinal start end
                                                       up-vec down-vec
                                                       min-dist)
                                  top-left-path)
          (should-be-almost-equal (2dg---path-cardinal start end
                                                       left-vec right-vec
                                                       min-dist)
                                  top-left-path)))
      ;; ;; straight shot
      (let* ((start (2dg-point- 0 0))
             (end (2dg-point- 5 0))
             (expected-path (list start end)))
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     up-vec up-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     down-vec down-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     up-vec down-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     down-vec up-vec
                                                     min-dist)
                                expected-path))
      ;; ;; U joint
      (let* ((start (2dg-point- 0 0))
             (end (2dg-point- 5 0))
             (expected-path (list start
                                  (2dg-point- 0 min-dist)
                                  (2dg-point- 5 min-dist)
                                  end)))
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     left-vec left-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     left-vec down-vec
                                                     min-dist)
                                expected-path))
      ;; jog - horizontal first.
      (let* ((start (2dg-point- 0 0))
             (end (2dg-point- 5 5))
             (expected-path (list start
                                  (2dg-point- 2.5 0.0)
                                  (2dg-point- 2.5 5.0)
                                  end)))
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     right-vec right-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     down-vec right-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (2dg---path-cardinal start end
                                                     right-vec down-vec
                                                     min-dist)
                                expected-path))
      ;; oddball cases that don't violate min-distance
      (let* ((start (2dg-point- 7 5))
             (end (2dg-point- 8 8))
             (expected-path (list start
                                  (2dg-point- 7.0 6.5)
                                  (2dg-point- 8.0 6.5)
                                  end)))
        (should-be-almost-equal (2dg---path-cardinal start end
                                                       up-vec up-vec
                                                       0.0)
                                expected-path))
      )))

(ert-deftest 2dg-path-append-simplify ()
  (cl-flet ((should-be-almost-equal
             (A B)
             (should
              (2dg-almost-equal (2dg-path :points A) (2dg-path :points B)))))
    ;; (let ((A (list (2dg-point :x 0.0 :y 0.0)
    ;;                (2dg-point :x 1.0 :y 0.0)
    ;;                (2dg-point :x 2.0 :y 0.0)))
    ;;       (B (list (2dg-point :x 2.0 :y 0.0)
    ;;                (2dg-point :x 2.0 :y 1.0)
    ;;                (2dg-point :x 2.0 :y 2.0))))
    ;;   (should-be-almost-equal (scxml---path-append-simplify A B)
    ;;                           (list (2dg-point :x 0.0 :y 0.0)
    ;;                                 (2dg-point :x 2.0 :y 0.0)
    ;;                                 (2dg-point :x 2.0 :y 2.0))))
    ;; (let ((A (list (2dg-point- -2 0)
    ;;                (2dg-point- -1 0)
    ;;                (2dg-point- 0 0)
    ;;                (2dg-point- 4 3)))
    ;;       (B (list (2dg-point- 4 3)
    ;;                (2dg-point- 8 6)
    ;;                (2dg-point- 8 6)
    ;;                (2dg-point- 9 6)
    ;;                (2dg-point- 10 6))))
    ;;   (should-be-almost-equal (scxml---path-append-simplify A B)
    ;;                           (list (2dg-point- -2 0)
    ;;                                 (2dg-point- 0 0)
    ;;                                 (2dg-point- 8 6)
    ;;                                 (2dg-point- 10 6))))
    ;; A few edge cases.
    (let ((A (list (2dg-point- 0 0)
                   (2dg-point- 1 0)
                   (2dg-point- 2 1)))
          (B (list (2dg-point- 2 1)
                   (2dg-point- 3 1)
                   (2dg-point- 3 1)
                   (2dg-point- 4 1)))
          (expected (list (2dg-point- 0 0)
                          (2dg-point- 1 0)
                          (2dg-point- 2 1)
                          (2dg-point- 4 1))))

      (should-be-almost-equal (2dg-simplified A B)
                              expected)
      (should (eq 3 (length A)))
      (should-be-almost-equal (2dg-simplified nil A B)
                              expected)
      (should-be-almost-equal (2dg-simplified nil A nil B nil)
                              expected)
      (should-be-almost-equal (2dg-simplified nil nil A nil B nil nil)
                              expected))))

(ert-deftest 2dg-path-nudge-path ()
  (let ((path-pts (list (2dg-point- 0 0)
                        (2dg-point- 1 0)
                        (2dg-point- 1 0)
                        (2dg-point- 2 0))))
    (should (2dg-almost-equal
             (2dg-path :points (2dg-nudge-path path-pts
                                               3
                                               (2dg-point- 0 0.4)))
             (2dg-path :points (list (2dg-point- 0 0)
                                       (2dg-point- 1 0)
                                       (2dg-point- 1 0.4)
                                       (2dg-point- 2 0.4))))))
  (let ((path-pts (list (2dg-point- 0 0)
                        (2dg-point- 1 0)
                        (2dg-point- 1 1)
                        (2dg-point- 2 1)
                        (2dg-point- 2 2))))
    (should (2dg-almost-equal
             (2dg-path :points (2dg-nudge-path path-pts
                                                     2
                                                     (2dg-point- 0.2 0.2)))
             (2dg-path :points (list (2dg-point- 0 0)
                                       (2dg-point- 1.2 0)
                                       (2dg-point- 1.2 1.2)
                                       (2dg-point- 2 1.2)
                                       (2dg-point- 2 2))))))
  (let ((path-pts (list (2dg-point- 0 0)
                        (2dg-point- 1 0)
                        (2dg-point- 1 1))))
    (should (2dg-almost-equal
             (2dg-path :points (2dg-nudge-path path-pts
                                               1
                                               (2dg-point- 0 0.1)))
             (2dg-path :points (list (2dg-point- 0 0.1)
                                     (2dg-point- 1 0.1)
                                     (2dg-point- 1 1)))))))

(ert-deftest 2dg-path-stretch ()
  (cl-flet ((should-be-almost-equal
             (A B)
             (should
              (2dg-almost-equal (2dg-path :points A) (2dg-path :points B)))))
    ;; normal stretches that don't need any special cases
    (progn
      (let* ((start-pt (2dg-point :x 0.0 :y 0.0))
             (points (list start-pt
                           (2dg-point :x 5.0 :y 0.0)
                           (2dg-point :x 5.0 :y 5.0))))
        (should-be-almost-equal (2dg---path-stretch points
                                                        start-pt
                                                        (2dg-point :x 10.0 :y 10.0))
                                (list start-pt
                                      (2dg-point :x 10.0 :y 0.0)
                                      (2dg-point :x 10.0 :y 10.0))))
      (let* ((start-pt (2dg-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (2dg-point :x 6.0 :y 1.0)
                           (2dg-point :x 6.0 :y 6.0))))
        (should-be-almost-equal (2dg---path-stretch points
                                                        (2dg-point :x 0.0 :y 0.0)
                                                        (2dg-point :x 10.0 :y 10.0))
                                (list (2dg-point :x 0.0 :y 0.0)
                                      (2dg-point :x 10.0 :y 0.0)
                                      (2dg-point :x 10.0 :y 10.0)))))

    ;; here are two impossible stretches
    (progn
      (let* ((start-pt (2dg-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (2dg-point :x 3.0 :y 1.0)))
             (force-start start-pt)
             (force-end (2dg-point :x 3.0 :y 3.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (2dg---path-stretch points
                                                      force-start
                                                      force-end)
                                (2dg---path-cardinal force-start
                                                       force-end
                                                       (2dg-point :x 1.0 :y 0.0)
                                                       (2dg-point :x 1.0 :y 0.0)
                                                       0.5)))
      (let* ((start-pt (2dg-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (2dg-point :x 1.0 :y 3.0)))
             (force-start start-pt)
             (force-end (2dg-point :x 3.0 :y 3.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (2dg---path-stretch points
                                                        force-start
                                                        force-end)
                                (2dg---path-cardinal force-start
                                                       force-end
                                                       (2dg-point :x 0.0 :y 1.0)
                                                       (2dg-point :x 0.0 :y 1.0)
                                                       0.5))))

    ;; displacement injection stretches
    (progn
      (let* ((start-pt (2dg-point :x 0.0 :y 0.0))
             (end-pt (2dg-point :x 3.0 :y 0.0))
             (points (list start-pt
                           (2dg-point :x 1.0 :y 0.0)
                           (2dg-point :x 1.0 :y 1.0)
                           (2dg-point :x 2.0 :y 1.0)
                           (2dg-point :x 2.0 :y 0.0)
                           end-pt))
             (force-start start-pt)
             (force-end (2dg-point :x 3.0 :y 1.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (2dg---path-stretch points
                                                        force-start
                                                        force-end)
                                (list force-start
                                      (2dg-point :x 1.0 :y 0.0)
                                      (2dg-point :x 1.0 :y 1.5)
                                      (2dg-point :x 2.0 :y 1.5)
                                      (2dg-point :x 2.0 :y 1.0)
                                      force-end))))

    ;; ;; weird stretch case
    ;; (progn
    ;;   (let* ((start-pt (2dg-point- 47 27))
    ;;          (end-pt (2dg-point- 50 14.5))
    ;;          (points (list start-pt
    ;;                        (2dg-point- 50 29)
    ;;                        (2dg-point- 51 29)
    ;;                        (2dg-point- 51 22.75)
    ;;                        (2dg-point- 50 16.5)
    ;;                        end-pt))
    ;;          (force-start (2dg-point- 46 27))
    ;;          (force-end end-pt))
    ;;     (should-be-almost-equal (2dg---path-stretch points
    ;;                                                 force-start
    ;;                                                 force-end)
    ;;                             (list force-start

    ;; HERE - fix this.
    ;; weird stretch I ran into that stopped being cardinal
    ;; (2dg-stretch '(#s(2dg-point 47.0 27.0) #s(2dg-point 50.0 29.0) #s(2dg-point 51.0 29.0) #s(2dg-point 51.0 22.75) #s(2dg-point 50.0 22.75) #s(2dg-point 50.0 16.5) #s(2dg-point 50.0 14.5))
    ;;          #s(2dg-point 46.0 27.0)
    ;;          #s(2dg-point 50.0 14.5))

    ))

(ert-deftest 2dg-path-cardinal-direction ()
  (let ((start-pt (2dg-point :x 0.0 :y 0.0))
        (right-vec (2dg-point :x 1.0 :y 0.0))
        (up-vec (2dg-point :x 0.0 :y 1.0))
        (down-vec (2dg-point :x 0.0 :y -1.0)))
    (should (2dg-almost-equal
             (2dg---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 1.0 :y 0.0))
             right-vec))
    (should (2dg-almost-equal
             (2dg---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 1.0 :y 1.0))
             right-vec))
    (should (2dg-almost-equal
             (2dg---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 1.0 :y -1.0))
             right-vec))
    (should (2dg-almost-equal
             (2dg---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 0.0 :y 1.0))
             up-vec))
    (should (2dg-almost-equal
             (2dg---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 0.0 :y -1.0))
             down-vec))
    (should (2dg-almost-equal
             (2dg---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x -1.0 :y 1.0))
             up-vec))
    (should (2dg-almost-equal
             (2dg---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x -1.0 :y -1.0))
             down-vec))
    (let ((totally-backwards (2dg---path-cardinal-direction start-pt
                                                              right-vec
                                                              (2dg-point :x -1.0 :y 0.0))))
      (should (or (2dg-almost-equal totally-backwards
                                      up-vec)
                  (2dg-almost-equal totally-backwards
                                      down-vec))))))

(ert-deftest 2dg-path-create-cardinal ()
  ;; ensure you're not allowed to create a cardinal path without actual cardinal data.
  (let ((invalid-cardinal-path (list (2dg-point- 0 0)
                                     (2dg-point- 1 0)
                                     (2dg-point- 2 1)))
        (found-expected-error nil))
    (condition-case nil
        (2dg-cardinal-path :points invalid-cardinal-path)
      (error (setq found-expected-error t)))
    (should found-expected-error)))

(ert-deftest 2dg-path-build-straight-line ()
  (let ((start (2dg-point- 1 1))
        (end (2dg-point- 2 2))
        (cardinal-end (2dg-point- 2 1)))
    (should (2dg-cardinal-path-p
             (2dg-build-path-straight-line start cardinal-end)))
    (should (2dg-path-p
             (2dg-build-path-straight-line start end)))))

(ert-deftest 2dg-slack-simplified ()
  (let ((input '(#s(2dg-point 54.0 22.0)
                 #s(2dg-point 56.5 22.0)
                 #s(2dg-point 56.5 21.0)
                 #s(2dg-point 59.0 21.0)
                 #s(2dg-point 59.0 23.0)))
        (expected (list (2dg-point- 54 22)
                        (2dg-point- 56.5 22)
                        (2dg-point- 59 22)
                        (2dg-point- 59 23))))
    (should (pts-almost-equal
             (2dg-slack-simplified input 2.0 2.0)
             expected)))
  (let ((input (list (2dg-point- 0 0)
                     (2dg-point- 1 0.1)
                     (2dg-point- 2 0.1)
                     (2dg-point- 3 0)))
        (expected (list (2dg-point- 0 0)
                        (2dg-point- 1 0)
                        (2dg-point- 2 0)
                        (2dg-point- 3 0))))
    (should (pts-almost-equal
             (2dg-slack-simplified input 1.0 1.0)
             expected)))
  (let ((input '(#s(2dg-point 54.0 22.0)
                 #s(2dg-point 55.5 22.0)
                 #s(2dg-point 55.5 21.0)
                 #s(2dg-point 57.0 21.0)
                 #s(2dg-point 57.0 23.0)))
        (expected (list (2dg-point- 54 22)
                        (2dg-point- 57 22)
                        (2dg-point- 57.0 23))))
    (should (pts-almost-equal
             (2dg-slack-simplified input 2.0 2.0)
             expected)))



  )

(provide '2dg-path-test)
