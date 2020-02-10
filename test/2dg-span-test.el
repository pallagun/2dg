(require 'ert)

(require '2dg-span)

(ert-deftest 2dg-span-contains-scalar ()
  (let ((span (2dg-span :start 1.0 :end 2.0))
        (left 1.0)
        (right 2.0)
        (middle 1.5))
    (should (2dg-contains span left))
    (should (2dg-contains span left nil))
    (should (2dg-contains span left 'stacked))
    (should-not (2dg-contains span left 'strict))

    (should (2dg-contains span middle))
    (should (2dg-contains span middle nil))
    (should (2dg-contains span middle 'stacked))
    (should (2dg-contains span middle 'strict))

    (should (2dg-contains span right))
    (should (2dg-contains span right nil))
    (should-not (2dg-contains span right 'stacked))
    (should-not (2dg-contains span right 'strict))))

(ert-deftest 2dg-span-contains-span ()
  (let ((span (2dg-span :start 1.0 :end 2.0)))
    (let ((inside (2dg-span :start 1.1 :end 1.9)))
      (should (2dg-contains span inside))
      (should (2dg-contains span inside nil))
      (should (2dg-contains span inside 'stacked))
      (should (2dg-contains span inside 'strict)))
    (let ((inside-flipped (2dg-span :start 1.9 :end 1.1)))
      (should (2dg-contains span inside-flipped))
      (should (2dg-contains span inside-flipped nil))
      (should (2dg-contains span inside-flipped 'stacked))
      (should (2dg-contains span inside-flipped 'strict)))
    (let ((not-strict (2dg-span :start 1.0 :end 1.9)))
      (should (2dg-contains span not-strict))
      (should (2dg-contains span not-strict nil))
      (should (2dg-contains span not-strict 'stacked))
      (should-not (2dg-contains span not-strict 'strict)))
    (let ((not-strict-flipped (2dg-span :start 1.9 :end 1.0)))
      (should (2dg-contains span not-strict-flipped))
      (should (2dg-contains span not-strict-flipped nil))
      (should (2dg-contains span not-strict-flipped 'stacked))
      (should-not (2dg-contains span not-strict-flipped 'strict)))))

(provide '2dg-span-test)
