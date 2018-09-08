(* These should all be false! *)
let strength_calculation = true

let check_target = true

let initial_difficulty = true

let transaction_replay = true

let fee_collection = true

let signature_hash_function = true

let private_key_generation = true

let randomness = true

let integration_tests = true

(* set this to true to enable snark verification *)
(* (the above flags, from key_generation down, must be false *)
let with_snark = false

(* Enabling this will SPEED UP builds as keys are generated at compile-time *)
let key_generation = not with_snark

let verify_blockchain = not with_snark

let compute_base_hash = not with_snark

let compute_base_proof = not with_snark

let extend_blockchain = not with_snark
