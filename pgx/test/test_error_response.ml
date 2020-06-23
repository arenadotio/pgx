open Pgx.Error_response

let tests =
  let info_msg =
    { code = "5"
    ; severity = "INFO"
    ; message = "test"
    ; custom = [ 'a', "string"; 'c', "field" ]
    }
  in
  let error_msg = { info_msg with severity = "ERROR" } in
  [ Alcotest.test_case "to_string tests: print msg when verbose = false" `Quick (fun () ->
        let verbose = false in
        Alcotest.(check string)
          "info to string"
          "INFO: 5: test"
          (to_string ~verbose info_msg);
        Alcotest.(check string)
          "error to string"
          "ERROR: 5: test"
          (to_string ~verbose error_msg))
  ; Alcotest.test_case
      "to_string tests: print msg and fields when verbose = true"
      `Quick
      (fun () ->
        let verbose = true in
        Alcotest.(check string)
          "vebose error to string"
          "ERROR: 5: test\na: string\nc: field"
          (to_string ~verbose error_msg))
  ; Alcotest.test_case
      "should_print tests: should not print when verbose = 0"
      `Quick
      (fun () ->
        let verbose = 0 in
        Alcotest.(check bool)
          "should not print info"
          false
          (should_print ~verbose info_msg);
        Alcotest.(check bool)
          "should not print error"
          false
          (should_print ~verbose error_msg))
  ; Alcotest.test_case
      "should_print tests: print if verbose = 1 and t.severity is one of three: INFO, \
       ERROR, PANIC"
      `Quick
      (fun () ->
        let verbose = 1 in
        [ "FATAL"; "ERROR"; "PANIC" ]
        |> List.iter (fun severity ->
               let msg = { info_msg with severity } in
               Alcotest.(check bool) "should print" true (should_print msg ~verbose));
        Alcotest.(check bool) "should not print" false (should_print info_msg ~verbose))
  ; Alcotest.test_case
      "should_print tests: print if verbose > 1 no matter t.severity"
      `Quick
      (fun () ->
        let verbose = 2 in
        [ "INFO"; "FATAL"; "ERROR"; "PANIC" ]
        |> List.iter (fun severity ->
               let msg = { info_msg with severity } in
               Alcotest.(check bool)
                 "should always print"
                 true
                 (should_print msg ~verbose)))
  ]
;;

let () =
  Alcotest.run "test_error_response" [ "to_string and should_print inline tests", tests ]
;;
