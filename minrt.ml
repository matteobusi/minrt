(****************************************************************)
(*                                                              *)
(* Ray Tracing Program for (Mini) Objective Caml                *)
(*                                                              *)
(* Original Program by Ryoji Kawamichi                          *)
(* Arranged for Chez Scheme by Motohico Nanano                  *)
(* Arranged for Objective Caml by Y. Oiwa and E. Sumii          *)
(*                                                              *)
(****************************************************************)

(* Originally from globals.ml *)
let objects = (let dummy = Array.make 0 0.0 in
    Array.make 60 (0, 0, 0, 0, dummy, dummy, false, dummy, dummy, dummy)) in


let size = Array.make 2 128 in


let dbg = Array.make 1 true in


let screen = Array.make 3 0.0 in


let vp = Array.make 3 0.0 in


let view = Array.make 3 0.0 in


let light = Array.make 3 0.0 in


let cos_v = Array.make 2 0.0 in


let sin_v = Array.make 2 0.0 in


let beam = Array.make 1 255.0 in


let and_net = Array.make 50 (Array.make 1 (-1)) in


let or_net = Array.make 1 (Array.make 1 and_net.(0)) in

let cs_temp = Array.make 16 0.0 in


let solver_dist = Array.make 1 0.0 in


let vscan = Array.make 3 0.0 in


let intsec_rectside = Array.make 1 0 in


let tmin = Array.make 1 1000000000.0 in


let crashed_point = Array.make 3 0.0 in


let crashed_object = Array.make 1 0 in


let end_flag = Array.make 1 false in


let viewpoint = Array.make 3 0.0 in


let nvector = Array.make 3 0.0 in


let rgb = Array.make 3 0.0 in


let texture_color = Array.make 3 0.0 in


let solver_w_vec = Array.make 3 0.0 in


let chkinside_p = Array.make 3 0.0 in


let isoutside_q = Array.make 3 0.0 in


let nvector_w = Array.make 3 0.0 in


let scan_d = Array.make 1 0.0 in


let scan_offset = Array.make 1 0.0 in


let scan_sscany = Array.make 1 0.0 in


let scan_met1 = Array.make 1 0.0 in


let wscan = Array.make 3 0.0 in


let rec xor (x : bool) (y : bool) : bool = if x then not y else y in


let rec fsqr (x : float) : float = x *. x in


let rec fhalf (x : float) : float = x /. 2.0 in


let rec o_texturetype (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : int =
  let (m_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
    m_tex
in


let rec o_form (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : int =
  let (xm_tex, m_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
    m_shape
in


let rec o_reflectiontype (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : int =
  let (xm_tex, xm_shape, m_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
    m_surface
in


let rec o_isinvert (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : bool =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, m_invert, xm_surfparams, xm_color, xm_rot123) = m in
    m_invert
in


let rec o_isrot (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : int =
  let (xm_tex, xm_shape, xm_surface, m_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
    m_isrot
in


let rec o_param_a (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, m_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
  m_abc.(0)
in


let rec o_param_b (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, m_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
  m_abc.(1)
in


let rec o_param_c (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, m_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
  m_abc.(2)
in


let rec o_param_x (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, m_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
  m_xyz.(0)
in


let rec o_param_y (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, m_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
  m_xyz.(1)
in


let rec o_param_z (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, m_xyz, xm_invert, xm_surfparams, xm_color, xm_rot123) = m in
  m_xyz.(2)
in


let rec o_diffuse (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, m_surfparams, xm_color, xm_rot123) = m in
  m_surfparams.(0)
in


let rec o_hilight (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, m_surfparams, xm_color, xm_rot123) = m in
  m_surfparams.(1)
in


let rec o_color_red (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, m_color, xm_rot123) = m in
  m_color.(0)
in


let rec o_color_green (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, m_color, xm_rot123) = m in
  m_color.(1)
in


let rec o_color_blue (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, m_color, xm_rot123) = m in
  m_color.(2)
in


let rec o_param_r1 (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, m_rot123) = m in
  m_rot123.(0)
in


let rec o_param_r2 (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, m_rot123) = m in
  m_rot123.(1)
in


let rec o_param_r3 (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : float =
  let (xm_tex, xm_shape, xm_surface, xm_isrot, xm_abc, xm_xyz, xm_invert, xm_surfparams, xm_color, m_rot123) = m in
  m_rot123.(2)
in


let rec normalize_vector (v : float array) (inv : bool) : unit =
  let n0 = sqrt (fsqr v.(0) +. fsqr v.(1) +. fsqr v.(2)) in
  let n = if inv then -.n0 else n0 in
  let _ = v.(0) <- v.(0) /. n in
  let _ = v.(1) <- v.(1) /. n in
  v.(2) <- v.(2) /. n
in


let rec sgn (x : float) : float =
  if not (x <= 0.0) then 1.0 else -1.0
in


let rec rad (x : float) : float = x *. 0.017453 in


let rec read_environ (_ : unit) : unit =
  let _ = screen.(0) <- read_float () in
  let _ = screen.(1) <- read_float () in
  let _ = screen.(2) <- read_float () in
  let v1 = rad (read_float ()) in
  let _ = cos_v.(0) <- cos v1 in
  let _ = sin_v.(0) <- sin v1 in
  let v2 = rad (read_float ()) in
  let _ = cos_v.(1) <- cos v2 in
  let _ = sin_v.(1) <- sin v2 in
  let _ = read_float () in
  let l1 = rad (read_float ()) in
  let sl1 = sin l1 in
  let _ = light.(1) <- -.sl1 in
  let l2 = rad (read_float ()) in
  let cl1 = cos l1 in
  let sl2 = sin l2 in
  let _ = light.(0) <- cl1 *. sl2 in
  let cl2 = cos l2 in
  let _ = light.(2) <- cl1 *. cl2 in
  let _ = beam.(0) <- read_float () in
  let _ = vp.(0) <- cos_v.(0) *. sin_v.(1) *. -200.0 in
  let _ = vp.(1) <- -.sin_v.(0) *. -200.0 in
  let _ = vp.(2) <- cos_v.(0) *. cos_v.(1) *. -200.0 in
  let _ = view.(0) <- vp.(0) +. screen.(0) in
  let _ = view.(1) <- vp.(1) +. screen.(1) in
  view.(2) <- vp.(2) +. screen.(2)
in


let rec read_nth_object (n : int) : bool =
  let texture = read_int () in
  if not (texture = -1) then
    let form = read_int () in
    let refltype = read_int () in
    let isrot_p = read_int () in
    let abc = Array.make 3 0.0 in
    let _ = abc.(0) <- read_float () in
    let _ = abc.(1) <- read_float () in
    let _ = abc.(2) <- read_float () in
    let xyz = Array.make 3 0.0 in
    let _ = xyz.(0) <- read_float () in
    let _ = xyz.(1) <- read_float () in
    let _ = xyz.(2) <- read_float () in
    let m_invert = not (0.0 <= read_float ()) in
    let reflparam = Array.make 2 0.0 in
    let _ = reflparam.(0) <- read_float () in
    let _ = reflparam.(1) <- read_float () in
    let color = Array.make 3 0.0 in
    let _ = color.(0) <- read_float () in
    let _ = color.(1) <- read_float () in
    let _ = color.(2) <- read_float () in
    let rotation = Array.make 3 0.0 in
    let _ =
      if not (isrot_p = 0) then
        let _ = rotation.(0) <- rad (read_float ()) in
        let _ = rotation.(1) <- rad (read_float ()) in
        rotation.(2) <- rad (read_float ())
      else ()
    in
    let m_invert2 = if form = 2 then true else m_invert in
    let obj =
      ( texture,
        form,
        refltype,
        isrot_p,
        abc,
        xyz,
        m_invert2,
        reflparam,
        color,
        rotation )
    in
    let _ = objects.(n) <- obj in
    let _ =
      if form = 3 then
        let a = abc.(0) in
        let _ =
          abc.(0) <- (if 0.0 = a then 0.0 else sgn a /. fsqr a)
        in
        let b = abc.(1) in
        let _ =
          abc.(1) <- (if 0.0 = b then 0.0 else sgn b /. fsqr b)
        in
        let c = abc.(2) in
        abc.(2) <- (if 0.0 = c then 0.0 else sgn c /. fsqr c)
      else if form = 2 then normalize_vector abc (not m_invert)
      else ()
    in
    let _ =
      if not (isrot_p = 0) then
        let _ = cs_temp.(10) <- cos rotation.(0) in
        let _ = cs_temp.(11) <- sin rotation.(0) in
        let _ = cs_temp.(12) <- cos rotation.(1) in
        let _ = cs_temp.(13) <- sin rotation.(1) in
        let _ = cs_temp.(14) <- cos rotation.(2) in
        let _ = cs_temp.(15) <- sin rotation.(2) in
        let _ = cs_temp.(0) <- cs_temp.(12) *. cs_temp.(14) in
        let _ =
          cs_temp.(1) <-
            (cs_temp.(11) *. cs_temp.(13) *. cs_temp.(14))
            -. (cs_temp.(10) *. cs_temp.(15))
        in
        let _ =
          cs_temp.(2) <-
            (cs_temp.(10) *. cs_temp.(13) *. cs_temp.(14))
            +. (cs_temp.(11) *. cs_temp.(15))
        in
        let _ = cs_temp.(3) <- cs_temp.(12) *. cs_temp.(15) in
        let _ =
          cs_temp.(4) <-
            (cs_temp.(11) *. cs_temp.(13) *. cs_temp.(15))
            +. (cs_temp.(10) *. cs_temp.(14))
        in
        let _ =
          cs_temp.(5) <-
            (cs_temp.(10) *. cs_temp.(13) *. cs_temp.(15))
            -. (cs_temp.(11) *. cs_temp.(14))
        in
        let _ = cs_temp.(6) <- -.cs_temp.(13) in
        let _ = cs_temp.(7) <- cs_temp.(11) *. cs_temp.(12) in
        let _ = cs_temp.(8) <- cs_temp.(10) *. cs_temp.(12) in
        let ao = abc.(0) in
        let bo = abc.(1) in
        let co = abc.(2) in
        let _ =
          abc.(0) <-
            (ao *. fsqr cs_temp.(0))
            +. (bo *. fsqr cs_temp.(3))
            +. (co *. fsqr cs_temp.(6))
        in
        let _ =
          abc.(1) <-
            (ao *. fsqr cs_temp.(1))
            +. (bo *. fsqr cs_temp.(4))
            +. (co *. fsqr cs_temp.(7))
        in
        let _ =
          abc.(2) <-
            (ao *. fsqr cs_temp.(2))
            +. (bo *. fsqr cs_temp.(5))
            +. (co *. fsqr cs_temp.(8))
        in
        let _ =
          rotation.(0) <-
            2.0
            *. ( (ao *. cs_temp.(1) *. cs_temp.(2))
               +. (bo *. cs_temp.(4) *. cs_temp.(5))
               +. (co *. cs_temp.(7) *. cs_temp.(8)) )
        in
        let _ =
          rotation.(1) <-
            2.0
            *. ( (ao *. cs_temp.(0) *. cs_temp.(2))
               +. (bo *. cs_temp.(3) *. cs_temp.(5))
               +. (co *. cs_temp.(6) *. cs_temp.(8)) )
        in
        rotation.(2) <-
          2.0
          *. ( (ao *. cs_temp.(0) *. cs_temp.(1))
             +. (bo *. cs_temp.(3) *. cs_temp.(4))
             +. (co *. cs_temp.(6) *. cs_temp.(7)) )
      else ()
    in
    true
  else false
in


let rec read_object (n : int) : unit =
  if not (61 <= n) then if read_nth_object n then read_object (n + 1) else ()
  else ()
in


let rec read_all_object (_ : unit) : unit = read_object 0 in


let rec read_net_item (length : int) : int array =
  let item = read_int () in
  if item = -1 then Array.make (length + 1) (-1)
  else
    let v = read_net_item (length + 1) in
    let _ = v.(length) <- item in
    v
in


let rec read_or_network (length : int) : (int array) array =
  let net = read_net_item 0 in
  if net.(0) = -1 then Array.make (length + 1) net
  else
    let v = read_or_network (length + 1) in
    let _ = v.(length) <- net in
    v
in


let rec read_and_network (n : int) : unit =
  let net = read_net_item 0 in
  if net.(0) = -1 then ()
  else
    let _ = and_net.(n) <- net in
    read_and_network (n + 1)
in


let rec read_parameter (_ : unit) : unit =
  let _ = read_environ () in
  let _ = read_all_object () in
  let _ = read_and_network 0 in
  or_net.(0) <- read_or_network 0
in


let rec solver_rect (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (l : float array) : int =
  let answera =
    if 0.0 = l.(0) then false
    else
      let d =
        if xor (o_isinvert m) (not (0.0 <= l.(0))) then o_param_a m
        else -.o_param_a m
      in
      let d2 = (d -. solver_w_vec.(0)) /. l.(0) in
      if not (o_param_b m <= abs_float ((d2 *. l.(1)) +. solver_w_vec.(1))) then
        if not (o_param_c m <= abs_float ((d2 *. l.(2)) +. solver_w_vec.(2)))
        then
          let _ = solver_dist.(0) <- d2 in
          true
        else false
      else false
  in
  if answera then 1
  else
    let answerb =
      if 0.0 = l.(1) then false
      else
        let d =
          if xor (o_isinvert m) (not (0.0 <= l.(1))) then o_param_b m
          else -.o_param_b m
        in
        let d2 = (d -. solver_w_vec.(1)) /. l.(1) in
        if not (o_param_c m <= abs_float ((d2 *. l.(2)) +. solver_w_vec.(2)))
        then
          if not (o_param_a m <= abs_float ((d2 *. l.(0)) +. solver_w_vec.(0)))
          then
            let _ = solver_dist.(0) <- d2 in
            true
          else false
        else false
    in
    if answerb then 2
    else
      let answerc =
        if 0.0 = l.(2) then false
        else
          let d =
            if xor (o_isinvert m) (not (0.0 <= l.(2))) then o_param_c m
            else -.o_param_c m
          in
          let d2 = (d -. solver_w_vec.(2)) /. l.(2) in
          if not (o_param_a m <= abs_float ((d2 *. l.(0)) +. solver_w_vec.(0)))
          then
            if not (o_param_b m <= abs_float ((d2 *. l.(1)) +. solver_w_vec.(1)))
            then
              let _ = solver_dist.(0) <- d2 in
              true
            else false
          else false
      in
      if answerc then 3 else 0
in


let rec solver_surface (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (l : float array) : int =
  let q =
    (l.(0) *. o_param_a m) +. (l.(1) *. o_param_b m) +. (l.(2) *. o_param_c m)
  in
  if not (q <= 0.0) then
    let t =
      ( (solver_w_vec.(0) *. o_param_a m)
      +. (solver_w_vec.(1) *. o_param_b m)
      +. (solver_w_vec.(2) *. o_param_c m) )
      /. q
    in
    let _ = solver_dist.(0) <- -.t in
    1
  else 0
in


let rec in_prod_sqr_obj (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (v : float array) : float =
  (fsqr v.(0) *. o_param_a m)
  +. (fsqr v.(1) *. o_param_b m)
  +. (fsqr v.(2) *. o_param_c m)
in


let rec in_prod_co_objrot (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (v : float array) : float =
  (v.(1) *. v.(2) *. o_param_r1 m)
  +. (v.(0) *. v.(2) *. o_param_r2 m)
  +. (v.(0) *. v.(1) *. o_param_r3 m)
in


let rec solver2nd_mul_b (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (l : float array) : float =
  (solver_w_vec.(0) *. l.(0) *. o_param_a m)
  +. (solver_w_vec.(1) *. l.(1) *. o_param_b m)
  +. (solver_w_vec.(2) *. l.(2) *. o_param_c m)
in


let rec solver2nd_rot_b (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (l : float array) : float =
  (((solver_w_vec.(2) *. l.(1)) +. (solver_w_vec.(1) *. l.(2))) *. o_param_r1 m)
  +. ((solver_w_vec.(0) *. l.(2)) +. (solver_w_vec.(2) *. l.(0)))
     *. o_param_r2 m
  +. ((solver_w_vec.(0) *. l.(1)) +. (solver_w_vec.(1) *. l.(0)))
     *. o_param_r3 m
in


let rec solver_second (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (l : float array) : int =
  let aa0 = in_prod_sqr_obj m l in
  let aa = if not (o_isrot m = 0) then aa0 +. in_prod_co_objrot m l else aa0 in
  if 0.0 = aa then 0
  else
    let bb0 = 2.0 *. solver2nd_mul_b m l in
    let bb = if not (o_isrot m = 0) then bb0 +. solver2nd_rot_b m l else bb0 in
    let cc0 = in_prod_sqr_obj m solver_w_vec in
    let cc1 =
      if not (o_isrot m = 0) then cc0 +. in_prod_co_objrot m solver_w_vec
      else cc0
    in
    let cc = if o_form m = 3 then cc1 -. 1.0 else cc1 in
    let d =
      let d2 = 4.0 *. aa *. cc in
      fsqr bb -. d2
    in
    if not (d <= 0.0) then
      let sd = sqrt d in
      let t1 = if o_isinvert m then sd else -.sd in
      let _ = solver_dist.(0) <- (t1 -. bb) /. 2.0 /. aa in
      1
    else 0
in


let rec solver (index : int) (l : float array) (p : float array) : int =
  let m = objects.(index) in
  let _ = solver_w_vec.(0) <- p.(0) -. o_param_x m in
  let _ = solver_w_vec.(1) <- p.(1) -. o_param_y m in
  let _ = solver_w_vec.(2) <- p.(2) -. o_param_z m in
  let m_shape = o_form m in
  if m_shape = 1 then solver_rect m l
  else if m_shape = 2 then solver_surface m l
  else solver_second m l
in


let rec is_rect_outside (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : bool =
  if
    if not (o_param_a m <= abs_float isoutside_q.(0)) then
      if not (o_param_b m <= abs_float isoutside_q.(1)) then
        if not (o_param_c m <= abs_float isoutside_q.(2)) then true else false
      else false
    else false
  then o_isinvert m
  else not (o_isinvert m)
in


let rec is_plane_outside (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : bool =
  let w =
    (o_param_a m *. isoutside_q.(0))
    +. (o_param_b m *. isoutside_q.(1))
    +. (o_param_c m *. isoutside_q.(2))
  in
  let s = not (0.0 <= w) in
  not (xor (o_isinvert m) s)
in


let rec is_second_outside (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : bool =
  let w = in_prod_sqr_obj m isoutside_q in
  let w2 = if o_form m = 3 then w -. 1.0 else w in
  let w3 =
    if not (o_isrot m = 0) then w2 +. in_prod_co_objrot m isoutside_q else w2
  in
  let s = not (0.0 <= w3) in
  not (xor (o_isinvert m) s)
in


let rec is_outside (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : bool =
  let _ = isoutside_q.(0) <- chkinside_p.(0) -. o_param_x m in
  let _ = isoutside_q.(1) <- chkinside_p.(1) -. o_param_y m in
  let _ = isoutside_q.(2) <- chkinside_p.(2) -. o_param_z m in
  let m_shape = o_form m in
  if m_shape = 1 then is_rect_outside m
  else if m_shape = 2 then is_plane_outside m
  else is_second_outside m
in


let rec check_all_inside (ofs : int) (iand : int array) : bool =
  let head = iand.(ofs) in
  if head = -1 then true
  else if is_outside objects.(head) then false
  else check_all_inside (ofs + 1) iand
in


let rec shadow_check_and_group (iand_ofs : int) (and_group : int array)
    (p : float array) : bool =
  if and_group.(iand_ofs) = -1 then false
  else
    let obj = and_group.(iand_ofs) in
    let t0 = solver obj light p in
    let t0p = solver_dist.(0) in
    if if not (t0 = 0) then not (-0.200000 <= t0p) else false then
      let t = t0p +. 0.010000 in
      let _ = chkinside_p.(0) <- (light.(0) *. t) +. p.(0) in
      let _ = chkinside_p.(1) <- (light.(1) *. t) +. p.(1) in
      let _ = chkinside_p.(2) <- (light.(2) *. t) +. p.(2) in
      if check_all_inside 0 and_group then true
      else shadow_check_and_group (iand_ofs + 1) and_group p
    else if o_isinvert objects.(obj) then
      shadow_check_and_group (iand_ofs + 1) and_group p
    else false
in


let rec shadow_check_one_or_group (ofs : int) (or_group : int array)
    (p : float array) : bool =
  let head = or_group.(ofs) in
  if head = -1 then false
  else
    let and_group = and_net.(head) in
    let shadow_p = shadow_check_and_group 0 and_group p in
    if shadow_p then true else shadow_check_one_or_group (ofs + 1) or_group p
in


let rec shadow_check_one_or_matrix (ofs : int) (or_matrix : int array array)
    (p : float array) : bool =
  let head = or_matrix.(ofs) in
  let range_primitive = head.(0) in
  if range_primitive = -1 then false
  else if range_primitive = 99 then
    if shadow_check_one_or_group 1 head p then true
    else shadow_check_one_or_matrix (ofs + 1) or_matrix p
  else
    let t = solver range_primitive light p in
    if not (t = 0) then
      if not (-0.100000 <= solver_dist.(0)) then
        if shadow_check_one_or_group 1 head p then true
        else shadow_check_one_or_matrix (ofs + 1) or_matrix p
      else shadow_check_one_or_matrix (ofs + 1) or_matrix p
    else shadow_check_one_or_matrix (ofs + 1) or_matrix p
in


let rec solve_each_element (iand_ofs : int) (and_group : int array) : unit =
  let iobj = and_group.(iand_ofs) in
  if iobj = -1 then ()
  else
    let t0 = solver iobj vscan viewpoint in
    let _ =
      if not (t0 = 0) then
        let t0p = solver_dist.(0) in
        if not (t0p <= -0.100000) then
          if not (tmin.(0) <= t0p) then
            let t = t0p +. 0.010000 in
            let _ = chkinside_p.(0) <- (vscan.(0) *. t) +. viewpoint.(0) in
            let _ = chkinside_p.(1) <- (vscan.(1) *. t) +. viewpoint.(1) in
            let _ = chkinside_p.(2) <- (vscan.(2) *. t) +. viewpoint.(2) in
            if check_all_inside 0 and_group then
              let _ = tmin.(0) <- t in
              let _ = crashed_point.(0) <- chkinside_p.(0) in
              let _ = crashed_point.(1) <- chkinside_p.(1) in
              let _ = crashed_point.(2) <- chkinside_p.(2) in
              let _ = intsec_rectside.(0) <- t0 in
              crashed_object.(0) <- iobj
            else ()
          else ()
        else ()
      else if o_isinvert objects.(iobj) then ()
      else end_flag.(0) <- true
    in
    if not end_flag.(0) then solve_each_element (iand_ofs + 1) and_group else ()
in


let rec solve_one_or_network (ofs : int) (or_group : int array) : unit =
  let head = or_group.(ofs) in
  if head = -1 then ()
  else
    let and_group = and_net.(head) in
    let _ = end_flag.(0) <- false in
    let _ = solve_each_element 0 and_group in
    solve_one_or_network (ofs + 1) or_group
in


let rec trace_or_matrix (ofs : int) (or_network : int array array) : unit =
  let head = or_network.(ofs) in
  let range_primitive = head.(0) in
  if range_primitive = -1 then ()
  else
    let _ =
      if range_primitive = 99 then solve_one_or_network 1 head
      else
        let t = solver range_primitive vscan viewpoint in
        if not (t = 0) then
          let tp = solver_dist.(0) in
          if not (tmin.(0) <= tp) then solve_one_or_network 1 head else ()
        else ()
    in
    trace_or_matrix (ofs + 1) or_network
in


let rec tracer (viewpoint : float array) (vscan : float array) : bool =
  let _ = tmin.(0) <- 1000000000.0 in
  let _ = trace_or_matrix 0 or_net.(0) in
  let t = tmin.(0) in
  if not (t <= -0.100000) then
    if not (100000000.0 <= t) then true else false
  else false
in


let rec get_nvector_rect (_ : unit) : unit =
  let rectside = intsec_rectside.(0) in
  if rectside = 1 then
    let _ = nvector.(0) <- -.sgn vscan.(0) in
    let _ = nvector.(1) <- 0.0 in
    nvector.(2) <- 0.0
  else if rectside = 2 then
    let _ = nvector.(0) <- 0.0 in
    let _ = nvector.(1) <- -.sgn vscan.(1) in
    nvector.(2) <- 0.0
  else if rectside = 3 then
    let _ = nvector.(0) <- 0.0 in
    let _ = nvector.(1) <- 0.0 in
    nvector.(2) <- -.sgn vscan.(2)
  else ()
in


let rec get_nvector_plane (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) : unit =
  let _ = nvector.(0) <- -.o_param_a m in
  let _ = nvector.(1) <- -.o_param_b m in
  nvector.(2) <- -.o_param_c m
in


let rec get_nvector_second_norot (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (p : float array) : unit =
  let _ = nvector.(0) <- (p.(0) -. o_param_x m) *. o_param_a m in
  let _ = nvector.(1) <- (p.(1) -. o_param_y m) *. o_param_b m in
  let _ = nvector.(2) <- (p.(2) -. o_param_z m) *. o_param_c m in
  normalize_vector nvector (o_isinvert m)
in


let rec get_nvector_second_rot (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (p : float array) : unit =
  let _ = nvector_w.(0) <- p.(0) -. o_param_x m in
  let _ = nvector_w.(1) <- p.(1) -. o_param_y m in
  let _ = nvector_w.(2) <- p.(2) -. o_param_z m in
  let _ =
    nvector.(0) <-
      (nvector_w.(0) *. o_param_a m)
      +. fhalf
           ((nvector_w.(1) *. o_param_r3 m) +. (nvector_w.(2) *. o_param_r2 m))
  in
  let _ =
    nvector.(1) <-
      (nvector_w.(1) *. o_param_b m)
      +. fhalf
           ((nvector_w.(0) *. o_param_r3 m) +. (nvector_w.(2) *. o_param_r1 m))
  in
  let _ =
    nvector.(2) <-
      (nvector_w.(2) *. o_param_c m)
      +. fhalf
           ((nvector_w.(0) *. o_param_r2 m) +. (nvector_w.(1) *. o_param_r1 m))
  in
  normalize_vector nvector (o_isinvert m)
in


let rec get_nvector (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (p : float array) : unit =
  let m_shape = o_form m in
  if m_shape = 1 then get_nvector_rect ()
  else if m_shape = 2 then get_nvector_plane m
  else if not (o_isrot m = 0) then get_nvector_second_rot m p
  else get_nvector_second_norot m p
in


let rec utexture (m : (int * int * int * int * float array * float array * bool * float array * float array * float array)) (p : float array) : unit =
  let m_tex = o_texturetype m in
  let _ = texture_color.(0) <- o_color_red m in
  let _ = texture_color.(1) <- o_color_green m in
  let _ = texture_color.(2) <- o_color_blue m in
  if m_tex = 1 then
    let w1 = p.(0) -. o_param_x m in
    let flag1 =
      let d1 = floor (w1 *. 0.050000) *. 20.0 in
      if not (10.0 <= w1 -. d1) then true else false
    in
    let w3 = p.(2) -. o_param_z m in
    let flag2 =
      let d2 = floor (w3 *. 0.050000) *. 20.0 in
      if not (10.0 <= w3 -. d2) then true else false
    in
    texture_color.(1) <-
      ( if flag1 then if flag2 then 255.0 else 0.0
      else if flag2 then 0.0
      else 255.0 )
  else if m_tex = 2 then
    let w2 = fsqr (sin (p.(1) *. 0.250000)) in
    let _ = texture_color.(0) <- 255.0 *. w2 in
    texture_color.(1) <- 255.0 *. (1.0 -. w2)
  else if m_tex = 3 then
    let w1 = p.(0) -. o_param_x m in
    let w3 = p.(2) -. o_param_z m in
    let w2 = sqrt (fsqr w1 +. fsqr w3) /. 10.0 in
    let w4 = (w2 -. floor w2) *. 3.141593 in
    let cws = fsqr (cos w4) in
    let _ = texture_color.(1) <- cws *. 255.0 in
    texture_color.(2) <- (1.0 -. cws) *. 255.0
  else if m_tex = 4 then
    let w1 = (p.(0) -. o_param_x m) *. sqrt (o_param_a m) in
    let w3 = (p.(2) -. o_param_z m) *. sqrt (o_param_c m) in
    let w4 = sqrt (fsqr w1 +. fsqr w3) in
    let w7 =
      if not (0.000100 <= abs_float w1) then 15.0
      else
        let w5 = abs_float (w3 /. w1) in
        atan w5 *. (30.0 /. 3.141593)
    in
    let w9 = w7 -. floor w7 in
    let w2 = (p.(1) -. o_param_y m) *. sqrt (o_param_b m) in
    let w8 =
      if not (0.000100 <= abs_float w7) then 15.0
      else
        let w6 = abs_float (w2 /. w4) in
        atan w6 *. (30.0 /. 3.141593)
    in
    let w10 = w8 -. floor w8 in
    let w11 = 0.150000 -. fsqr (0.500000 -. w9) -. fsqr (0.500000 -. w10) in
    texture_color.(2) <-
      (if w11 <= 0.0 then 0.0 else w11 *. (255.0 /. 0.300000))
  else ()
in


let rec in_prod (v1 : float array) (v2 : float array) : float =
  (v1.(0) *. v2.(0)) +. (v1.(1) *. v2.(1)) +. (v1.(2) *. v2.(2))
in


let rec accumulate_vec_mul (v1 : float array) (v2 : float array) (w : float) :
    unit =
  let _ = v1.(0) <- v1.(0) +. (w *. v2.(0)) in
  let _ = v1.(1) <- v1.(1) +. (w *. v2.(1)) in
  v1.(2) <- v1.(2) +. (w *. v2.(2))
in


let rec raytracing (nref : int) (energy : float) : unit =
  let crashed_p = tracer viewpoint vscan in
  let _ =
    if not crashed_p then
      if not (nref = 0) then
        let hl = -.in_prod vscan light in
        if not (hl <= 0.0) then
          let ihl = fsqr hl *. hl *. energy *. beam.(0) in
          let _ = rgb.(0) <- rgb.(0) +. ihl in
          let _ = rgb.(1) <- rgb.(1) +. ihl in
          rgb.(2) <- rgb.(2) +. ihl
        else ()
      else ()
    else ()
  in
  if crashed_p then
    let cobj = objects.(crashed_object.(0)) in
    let _ = get_nvector cobj crashed_point in
    let bright =
      if shadow_check_one_or_matrix 0 or_net.(0) crashed_point then 0.0
      else
        let br = -.in_prod nvector light in
        let br1 = if not (0.0 <= br) then 0.200000 else br +. 0.200000 in
        br1 *. energy *. o_diffuse cobj
    in
    let _ = utexture cobj crashed_point in
    let _ = accumulate_vec_mul rgb texture_color bright in
    if not (nref <= 4) then ()
    else if not (energy <= 0.100000) then
      let w = -2.0 *. in_prod vscan nvector in
      let _ = accumulate_vec_mul vscan nvector w in
      let m_surface = o_reflectiontype cobj in
      if m_surface = 1 then
        if 0.0 = o_hilight cobj then ()
        else
          let hl = -.in_prod vscan light in
          if not (hl <= 0.0) then
            let ihl = fsqr (fsqr hl) *. energy *. bright *. o_hilight cobj in
            let _ = rgb.(0) <- rgb.(0) +. ihl in
            let _ = rgb.(1) <- rgb.(1) +. ihl in
            rgb.(2) <- rgb.(2) +. ihl
          else ()
      else if m_surface = 2 then
        let _ = viewpoint.(0) <- crashed_point.(0) in
        let _ = viewpoint.(1) <- crashed_point.(1) in
        let _ = viewpoint.(2) <- crashed_point.(2) in
        let energy2 = energy *. (1.0 -. o_diffuse cobj) in
        raytracing (nref + 1) energy2
      else ()
    else ()
  else ()
in


let rec write_rgb (_ : unit) : unit =
  let rec cc (v : int) : int = (if v <= 255 then v else 255) in
    print_byte (cc (int_of_float rgb.(0))); print_byte (cc (int_of_float rgb.(1))); print_byte (cc (int_of_float rgb.(2)))
in


let rec write_ppm_header (_ : unit) : unit =
  let _ = print_byte 80 in
  let _ = print_byte (48 + 6) in
  let _ = print_byte 10 in
  let _ = print_int size.(0) in
  let _ = print_byte 32 in
  let _ = print_int size.(1) in
  let _ = print_byte 10 in
  let _ = print_int 255 in
  print_byte 10
in


let rec scan_point (scanx : int) : unit =
  if size.(0) <= scanx then ()
  else
    let sscanx = (float_of_int scanx -. scan_offset.(0)) *. scan_d.(0) in
      (vscan.(0) <- (sscanx *. cos_v.(1)) +. wscan.(0);
      vscan.(1) <- (scan_sscany.(0) *. cos_v.(0)) -. vp.(1);
      vscan.(2) <- (-.sscanx *. sin_v.(1)) +. wscan.(2);
      let metric = sqrt (fsqr sscanx +. scan_met1.(0)) in
        (vscan.(0) <- vscan.(0) /. metric;
        vscan.(1) <- vscan.(1) /. metric;
        vscan.(2) <- vscan.(2) /. metric;
        viewpoint.(0) <- view.(0);
        viewpoint.(1) <- view.(1);
        viewpoint.(2) <- view.(2);
        rgb.(0) <- 0.0;
        rgb.(1) <- 0.0;
        rgb.(2) <- 0.0;
        raytracing 0 1.0;
        write_rgb ();
        scan_point (scanx + 1)))
in


let rec scan_line (scany : int) : unit =
  if not (size.(0) <= scany) then
    (scan_sscany.(0) <- (let t = scan_offset.(0) -. 1.0 -. float_of_int scany in scan_d.(0) *. t);
    scan_met1.(0) <- fsqr scan_sscany.(0) +. 40000.0;
    let t1 = scan_sscany.(0) *. sin_v.(0) in
      wscan.(0) <- (t1 *. sin_v.(1)) -. vp.(0);
      wscan.(2) <- (t1 *. cos_v.(1)) -. vp.(2);
      scan_point 0;
      scan_line (scany + 1))
  else ()
in


let rec scan_start (_ : unit) : unit =
  write_ppm_header ();
  let sizex = float_of_int size.(0) in
    scan_d.(0) <- 128.0 /. sizex;
    scan_offset.(0) <- sizex /. 2.0;
    scan_line 0
in


let rec rt (size_x : int) (size_y : int) (debug_p : bool) : unit =
  size.(0) <- size_x;
  size.(1) <- size_y;
  dbg.(0) <- debug_p;
  read_parameter ();
  scan_start ()
in
rt 768 768 false
