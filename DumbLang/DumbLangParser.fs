namespace DumbLang

type DumbLangParser() =
    inherit BaseParser()

    let mutable count = 0

    override this.onNamed (name, label) =
        count <- count + 1
        base.onNamed (name, label)

    override this.onLabel str =
        let newStr = (String.replicate count "s") + str
        base.onLabel newStr