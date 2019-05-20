#!/usr/bin/env bats

ROSA=$BATS_TEST_DIRNAME/../result/bin/rosa

cd $BATS_TEST_DIRNAME

teardown() {
  rm out.s out
}

@test "should return right exit code" {
  run $ROSA "ikjyshgw.c"
  run out
  [ "$status" -eq 33 ]
}

@test "should return binary complement" {
  run $ROSA "isrxvzir.c"
  run out
  [ "$status" -eq 222 ]
}

@test "should return logical complement of zero" {
    run $ROSA "oekbwkmw.c"
    run out
    [ "$status" -eq 1 ]
}

@test "should return logical complement of non-zero" {
    run $ROSA "zybaedmp.c"
    run out
    [ "$status" -eq 0 ]
}

@test "should return additive complement" {
    run $ROSA "umgnlful.c"
    run out
    [ "$status" -eq 223 ]
}

@test "should sum left associative expression" {
    run $ROSA "ovnfmwuv.c"
    run out
    [ "$status" -eq 52 ]
}

@test "should sum right associative expression" {
    run $ROSA "fyikhpak.c"
    run out
    [ "$status" -eq 52 ]
}

@test "should subtract left associative expression" {
    run $ROSA "qgynsxke.c"
    run out
    [ "$status" -eq 16 ]
}

@test "should subtract right associative expression" {
    run $ROSA "znvgftiv.c"
    run out
    [ "$status" -eq 68 ]
}
