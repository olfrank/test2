import Hash       "mo:base/Hash";
import Map        "mo:base/HashMap";
import Principal  "mo:base/Principal";
import Nat        "mo:base/Nat";
import Nat32      "mo:base/Nat32";
import Nat64      "mo:base/Nat64";
import Text       "mo:base/Text";
import Iter       "mo:base/Iter";
import Float      "mo:base/Float";
import T          "./types";
import Ledger     "canister:ledger";
import Account    "./account";
import Time       "mo:base/Time";
import Int        "mo:base/Int";
import Error      "mo:base/Error";
import Debug      "mo:base/Debug";
import Result     "mo:base/Result";
import U          "./utils";
import Hex        "./Hex";
import Blob       "mo:base/Blob";


actor Tipping {

    type ArtistID = T.ArtistID;
    type UserID = T.UserID;
    type AdminID = T.AdminID;
    type AccountIdentifier = T.AccountIdentifier;
    type ICPTs = T.ICPTs;
    let TRAX_ACCOUNT = "2l26f-kcxq2-2rwa7-zy36b-3wive-m3hfd-xrbr4-gocr4-7rklt-gmj4y-nqe";
    let FEE : Nat64 = 10_000;

    // artist address -> user address -> amount 
    var tippingMap = Map.HashMap<ArtistID, Map.HashMap<UserID, Nat64>>(1, Principal.equal, Principal.hash);
    // total amount recieved from tips 
    var artistTotalMap = Map.HashMap<ArtistID, Nat64>(1, Principal.equal, Principal.hash);






// #region -TIPPING Changing state 
  private func putTippingMap(artist: ArtistID, status: Map.HashMap<UserID, Nat64>){
            tippingMap.put(artist, status);
  };
  private func putArtistTotal(artist: ArtistID, amount: Nat64){
            artistTotalMap.put(artist, amount);
  };

  private func replaceTippingMap(artist: ArtistID, amount: Nat64, user: UserID): async (?Nat64){ // if returned == 0 (this function could not find key value pair)
    switch(tippingMap.get(artist)){
        case(?nestedMap){
            switch(nestedMap.get(user)){
            case(?exists){
                nestedMap.replace(user, amount)
            };
            case null {?Nat64.fromNat(0)}
            };
        };
        case null {?Nat64.fromNat(0)}
    };
  };

  private func replaceArtistTotal(artist: ArtistID, amount: Nat64): async (?Nat64){ // if returned == 0 (this function could not find key value pair)
    artistTotalMap.replace(artist, amount)
  };         

// #endregion
  

    

// #region Transfer
  public func sendTip(from: UserID, to: ArtistID, amount: Nat64) : async (){

    var amountToSend = await platformDeduction(from, amount);
    Debug.print("HEREEEE");

        switch(await transfer(from, to, amountToSend)){

          case(#ok(res)){

            switch(artistTotalMap.get(to)){
                case(?exists){
                    var replaceWorked = await replaceArtistTotal(to, amountToSend);
                }; case null {
                    putArtistTotal(to, amountToSend);
                };
            };

            
            switch(tippingMap.get(to)){
                case(?exists){
                    var worked = await replaceTippingMap(to, amountToSend, from);
                    if(worked == ?Nat64.fromNat(0)){
                        Debug.print("DID NOT update tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    }else{
                        Debug.print("UPDATED tipMapping for artist: " # debug_show to # " in block " # debug_show res);
                    }
                };
                case null {
                    var x = Map.HashMap<Principal, Nat64>(2, Principal.equal, Principal.hash);
                    x.put(from, amountToSend);
                    putTippingMap(to, x);
                };
            };
           

            Debug.print("Paid artist: " # debug_show to # " in block " # debug_show res);
          }; case(#err(msg)){
            throw Error.reject("Unexpected error: " # debug_show msg);
          };
        };
    };



    func transfer(from: Principal, to: Principal, amount: Nat64): async Result.Result<Nat64, Text>{

        Debug.print("from subaccount: "# debug_show Account.principalToSubaccount(from));
        Debug.print("from subaccount: "# debug_show Account.accountIdentifier(from, Account.defaultSubaccount()));

        let now = Time.now();
        let res = await Ledger.transfer({
              memo = Nat64.fromNat(0); //TODO: add contentID as metadata for memo
              // ?Account.accountIdentifier(from, Account.defaultSubaccount());
              from_subaccount = ?Account.principalToSubaccount(from);
              to = Account.accountIdentifier(to, Account.defaultSubaccount());
              amount = { e8s = amount };
              fee = { e8s = FEE };
              created_at_time = ?{ timestamp_nanos = Nat64.fromNat(Int.abs(now)) };
            });

            Debug.print("res: "# debug_show res);

            switch (res) {
              case (#Ok(blockIndex)) {
                Debug.print("Paid recipient: " # debug_show to # " in block " # debug_show blockIndex);
                return #ok(blockIndex);
              };
              case (#Err(#InsufficientFunds { balance })) {
                throw Error.reject("Insufficient balance of " # debug_show balance # " from account:" # debug_show from # "");
              };
              case (#Err(other)) {
                throw Error.reject("Unexpected error: " # debug_show other);
              };
            };
    };



    private func platformDeduction(user: UserID, amount : Nat64) : async Nat64 {
        let traxAccount: Principal = Principal.fromText(TRAX_ACCOUNT);

        let amountFloat : Float = Float.fromInt(Nat64.toNat(amount));
        let deduction :  Float = amountFloat * 0.10;
        Debug.print("HEREEEE3");
        switch(await transfer(user, traxAccount, Nat64.fromNat(Int.abs(Float.toInt(deduction))))){
          case(#ok(res)){
            Debug.print("Fee paid to trax account: " # debug_show traxAccount # " in block " # debug_show res);
          };case(#err(msg)){
            throw Error.reject("Unexpected error: " # debug_show msg);
          };
        };

        return Nat64.fromNat(Int.abs(Float.toInt(amountFloat - deduction)));
    };
// #endregion


//#region - Query funcs
    public func getTippingMap(artist: ArtistID, user: UserID) : async ?Nat64 {
            switch(tippingMap.get(artist)){
              case(?nestedMap){
                nestedMap.get(user)
              };
              case null{
                return ?Nat64.fromNat(0)
              }
            };
    };
    public func getArtistTotalMap(artist: ArtistID) : async ?Nat64 {
            artistTotalMap.get(artist);
    };
//#endregion



}