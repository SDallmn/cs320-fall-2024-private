open Utils




  let lex (input: string) : tok list option =
    let words = split input in
    let rec process_tokens = function
      | [] -> Some []  
      | word :: rest -> 
        match tok_of_string_opt word with
        | Some token -> 
          (match process_tokens rest with
           | Some tokens -> Some (token :: tokens) 
           | None -> None) 
        | None -> None  
    in
    process_tokens words  