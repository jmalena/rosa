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

@test "should sum simple expression" {
  run $ROSA "igeuwctk.c"
  run out
  [ "$status" -eq 5 ]
}

@test "should sum complex left associative expression" {
    run $ROSA "ovnfmwuv.c"
    run out
    [ "$status" -eq 52 ]
}

@test "should sum complex right associative expression" {
    run $ROSA "fyikhpak.c"
    run out
    [ "$status" -eq 52 ]
}

@test "should subtract simple expression" {
  run $ROSA "ecjlkevc.c"
  run out
  [ "$status" -eq 2 ]
}

@test "should subtract complex left associative expression" {
    run $ROSA "qgynsxke.c"
    run out
    [ "$status" -eq 16 ]
}

@test "should subtract complex right associative expression" {
    run $ROSA "znvgftiv.c"
    run out
    [ "$status" -eq 68 ]
}

@test "should compare (==) true expression" {
  run $ROSA "ycyuvpsp.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (==) false expression" {
  run $ROSA "enjqmlho.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (!=) true expression" {
  run $ROSA "fglnfnxc.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (!=) false expression" {
  run $ROSA "xdnecxob.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (<=) true expression [1]" {
  run $ROSA "thslsbiv.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (<=) true expression [2]" {
  run $ROSA "zkrcwezf.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (<=) false expression" {
  run $ROSA "mrtvyrkp.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (<) true expression" {
  run $ROSA "rtesruzp.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (<) false expression" {
  run $ROSA "rciowgcu.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (>=) true expression [1]" {
  run $ROSA "jpejgnxc.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (>=) true expression [2]" {
  run $ROSA "btldwosp.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (>=) false expression" {
  run $ROSA "mxptuztj.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (>) true expression" {
  run $ROSA "yumjapwo.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (>) false expression" {
  run $ROSA "kkgsvdyy.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (&&) simple true expression" {
  run $ROSA "ofqpmhqb.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (&&) simple false expression" {
  run $ROSA "ilrsylzt.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (&&) complex left associative true expression" {
  run $ROSA "fhvbevqu.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (&&) complex left associative simple false expression" {
  run $ROSA "ralwfmpa.c"
  run out
  [ "$status" -eq 0 ]
}

@test "should compare (&&) complex right associative true expression" {
  run $ROSA "dzondczk.c"
  run out
  [ "$status" -eq 1 ]
}

@test "should compare (&&) complex right associative simple false expression" {
  run $ROSA "lhvmqfpj.c"
  run out
  [ "$status" -eq 0 ]
}
