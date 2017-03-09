include Multiparty

type title = string
type quote = float
type address = string
type date = int

type global =
    [ `msg of
        ([ `Buyer1 ], [ `Seller ]) dir * title *
    [ `msg of
        ([ `Seller ], [ `Buyer1 ]) dir * quote *
    [ `msg of
        ([ `Seller ], [ `Buyer2 ]) dir * quote *
    [ `msg of
        ([ `Buyer1 ], [ `Buyer2 ]) dir * quote *
    [ `branch of
        ([ `Buyer2 ], [ `Seller ]) dir *
            [ `ok of
                [ `msg of ([ `Buyer2 ], [ `Seller ]) dir * address *
                [ `msg of ([ `Seller ], [ `Buyer2 ]) dir * date *
                            [ `close ] ]]
            | `quit of
                [ `close ]]]]]]]
                                          

let ch : global channel = new_channel ()

module B1 = Local(struct type me = [`Buyer1] and them = [`Buyer2|`Seller] end)

let buyer1 () =
  B1.connect ch >>
  B1.send `Seller "Sing" >>
  B1.recv `Seller >>= fun quot ->
  (B1.ignore_msg :> ([`Seller],[`Buyer2],'a,'b,'c) B1.msg) >>
  B1.send `Buyer2 (quot /. 2.) >>
  (B1.ignore_branch (fun x -> `quit x) :> ([`Buyer2],[`Seller],'g,'h,'i) B1.branch) >>
  B1.close ()


module B2 = Local(struct type me = [`Buyer2] and them = [`Buyer1|`Seller] end)

let buyer2 () =
  B2.connect ch >>
  (B2.ignore_msg :> ([`Buyer1],[`Seller],'a,'b,'c) B2.msg) >>
  (B2.ignore_msg :> ([`Seller],[`Buyer1],'d,'e,'f) B2.msg) >>
  B2.recv `Seller >>= fun seller_quot ->
  B2.recv `Buyer1 >>= fun buyer1_quot ->
  if buyer1_quot < 20.00 then
    B2._select `Seller (fun x -> `ok x) >>
    B2.send `Seller "Nagoya, Japan" >>
    B2.recv `Seller >>= fun date ->
    B2.close ()
  else
    B2._select `Seller (fun x -> `quit x) >>
    B2.close ()

module S = Local(struct type me = [`Seller] and them = [`Buyer1|`Buyer2] end)

let seller () =
  S.connect ch >>
  S.recv `Buyer1 >>= fun title ->
  let price = 15.00 in
  S.send `Buyer1 price >>
  S.send `Buyer2 price >>
  (S.ignore_msg :> ([`Buyer1],[`Buyer2],'a,'b,'c) S.msg) >>
  S._branch_start `Buyer2 begin function
  | `ok(s) -> S._branch s `Buyer2 (S.recv `Buyer2 >>= fun address -> S.send `Buyer2 20170315 >> S.close ())
  | `quit(s) -> S._branch s `Buyer2 (S.close ())
  end
