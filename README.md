# Brindle

Brindle is a micro CNL for embedding calculations within text documents. It's based on [Glimpse](https://github.com/rgscherf/glimpse).

Rather than asking users to write an entire document in Glimpse and escaping descriptive passages with comments, Brindle only engages the compiler in passages where it's specifically invoked. Everything around Brindle passages is just English (or any language of your choice).

You can invoke Brindle with the phrase `This section is a computer program.` When you're finished writing Brindle clauses, drop back to natural language with `This is the end of the computer program.`

Behind the scenes, the Brindle compiler simply removes all text outside the invocation boundaries then evaluates all detected Brindle programs in sequence.

To evaluate a text containing Brindle programs, run `(brindle-eval text input-data)` in ns `core`. `input-data` should be an EDN map. Brindle will return a sequence of maps, each containing the input data at key `:data`, along with (possibly empty) sequences of `:errors` and `:warnings`.
