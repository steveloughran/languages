! Created by ${USER_NAME} on 20/02/2018.

PROGRAM newton
  REAL root
  REAL square
  INTEGER count, i
  square = 2
  root = 1
  count = 4
  ! iteration through DO
  DO 10 I = 1, 4
    root = (root + square / root) / 2
  10 END DO
  PRINT *, root

END PROGRAM newton