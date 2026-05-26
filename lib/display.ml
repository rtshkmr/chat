module SMeta = Session_meta

let pp_timestamp fmt ts =
  let tm = Unix.localtime ts in
  Format.fprintf fmt "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

let pp_prompt fmt ts = Format.fprintf fmt "[%a]" pp_timestamp ts
let now () = Unix.gettimeofday ()
let pp_prompt_now fmt () = pp_prompt fmt (now ())
let pp_warning fmt reason = Format.fprintf fmt "[WARNING] %s." reason
let eprintf_pp pp v = Lwt_io.eprintf "%s\n" (Format.asprintf "%a" pp v)
let write_pp oc pp v = Lwt_io.write oc (Format.asprintf "%a" pp v)

let pp_peer fmt { SMeta.handle; addr; port } =
  Format.fprintf fmt "%s@%s:%d" handle addr port

let pp_banner fmt { SMeta.me; them; connected_at } =
  Format.fprintf fmt "◉ %a <chat> %a → %a\n" pp_prompt connected_at pp_peer me
    pp_peer them

let pp_dummy_header fmt () =
  Format.fprintf fmt "◉ %a <chat> [dummy test] local → remote" pp_prompt_now ()
