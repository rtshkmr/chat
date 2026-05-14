open Alcotest

let test_add () = check int "adds correctly" 5 5
let () = run "test-chat" [ ("math", [ test_case "add works" `Quick test_add ]) ]
