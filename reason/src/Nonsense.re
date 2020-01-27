type state =
  | Error
  | Words(list(string));

[@react.component]
let make = _ => {
  Random.self_init();

  let (state, setState) = React.useState(_ => Words([]));

  let gotWords = (words': list(string)): unit =>
    setState(state =>
      switch (state) {
      | Error => Error
      | Words(words) => Words(List.append(words, words'))
      }
    );

  let gotWord = (set: string, word: string) =>
    switch (set) {
    | "nouns"
    | "objects" =>
      switch (Random.int(2)) {
      | 0 => gotWords(["the", word])
      | _ when List.mem(String.get(word, 0), ['a', 'e', 'i', 'o', 'u']) => gotWords(["an", word])
      | _ => gotWords(["a", word])
      }
    | _ => gotWords([word])
    };

  let decodeWord = (json: Js.Json.t): option(string) =>
    switch (Js.Json.decodeObject(json)) {
    | None => None
    | Some(obj) =>
      switch (Js.Dict.get(obj, "words")) {
      | None => None
      | Some(words) =>
        switch (Js.Json.decodeArray(words)) {
        | None => None
        | Some(words) =>
          switch (Js.Json.decodeString(words[0])) {
          | None => None
          | Some(word) => Some(word)
          }
        }
      }
    }

  let getWord = (~set: string) => {
    Fetch.fetch("https://api.noopschallenge.com/wordbot?set=" ++ set)
    |> Js.Promise.then_(Fetch.Response.json)
    |> Js.Promise.then_(json => {
         switch (decodeWord(json)) {
         | None => setState(_ => Error)
         | Some(word) => gotWord(set, word)
         };
         Js.Promise.resolve();
       })
  };

  React.useEffect0(() => {
    getWord(~set="nouns")
    |> Js.Promise.then_(_ => getWord(~set="verbs_past"))
    |> Js.Promise.then_(_ => getWord(~set="objects"))
    |> Js.Promise.then_(_ =>
         switch (Random.int(100)) {
         | n when n < 30 => getWord(~set="adverbs")
         | _ => Js.Promise.resolve()
         }
       )
    |> ignore;
    None;
  });

  <div>
    {switch (state) {
     | Error => React.string("Error")
     | Words([]) => React.string("Loading...")
     | Words(words) => React.string(String.concat(" ", words))
    }}
  </div>;
};
