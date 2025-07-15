open OUnit2
open Vec3

let test_components _ =
  let v = (1, 2, 3) in
  assert_equal 1 (x v);
  assert_equal 2 (y v);
  assert_equal 3 (z v)

let suite = "vec3 tests" >::: [ "components" >:: test_components ]
let () = run_test_tt_main suite
