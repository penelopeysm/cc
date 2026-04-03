let print_usage_and_exit (exec_name: string) =
  Printf.eprintf "Usage: %s <file.c>\n" (Filename.basename exec_name);
  exit 1
;;

let replace_extension (filename: string) (new_ext: string) : string =
  (Filename.remove_extension filename) ^ new_ext 

let try_compilation_stage (command: string) (stage_name: string) : unit =
  let exit_code = Sys.command command in
  if exit_code <> 0 then begin
    Printf.eprintf "Error: %s failed with exit code %d\n" stage_name exit_code;
    exit exit_code
  end
;;

let main () = 
  let args = Sys.argv in
  (* TODO: we could have some nice options here, like whether to clean up the
  assembly & object files, and whether to run the executable after compilation.
  *)
  if Array.length args <> 2 then print_usage_and_exit args.(0);
  let fname = args.(1) in
  if not (Filename.extension fname = ".c") then print_usage_and_exit args.(0);

  (* Get name of preprocessed file *)
  let preproc_fname = replace_extension fname ".i" in
  let preproc_command = Printf.sprintf "clang -E -P %s -o %s" fname preproc_fname in
  try_compilation_stage preproc_command "Preprocessing";

  (* Run our own compiler! *)
  let assembly_fname = replace_extension fname ".s" in
  let compiler_command = (Printf.sprintf "clang -S -O %s -o %s" preproc_fname assembly_fname) in
  try_compilation_stage compiler_command "Compilation";
  Sys.remove preproc_fname;

  (* Run the assembler and linker in one shot *)
  let executable_fname = Filename.remove_extension fname in
  let link_command = (Printf.sprintf "clang %s -o %s" assembly_fname executable_fname) in
  try_compilation_stage link_command "Assembling and linking";

  exit 0
;;


let () = main ()
