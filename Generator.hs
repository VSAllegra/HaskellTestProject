module Generator where

    import MyRandom (randomInt, pickRandom, probability)

    nouns :: [String]
    nouns = ["boy", "girl", "dog", "cat", "table", "chair", "cake"]
    verbs :: [String]
    verbs = ["hit", "threw", "pushed", "jumped", "ate", "spun"]
    articles :: [String]
    articles = ["a", "the"]
    conjunctions :: [String]
    conjunctions = ["for", "and", "nor", "but", "or", "yet", "so"]
    prepostions :: [String]
    prepostions = ["with", "to", "from", "on", "below", "above", "beside"]
    adjectives :: [String]
    adjectives = ["big", "small", "short", "tall", "red", "blue", "yellow", "green"]

    adjective_phrase :: IO String
    adjective_phrase = do
        adj <- pickRandom adjectives
        prob <- probability 0.25
        if prob then do
            phrase <- adjective_phrase
            return (adj ++ " " ++ phrase)
        else do
            return adj
    
    noun_phrase :: IO String
    noun_phrase = do
        art  <- pickRandom articles
        prob <- probability 0.25
        noun <- pickRandom nouns
        if prob then do
            phrase <- adjective_phrase
            return (art ++ " " ++ phrase ++ " " ++ noun)
        else do 
            return (art ++ " " ++ noun)

    prepositional_phrase :: IO String
    prepositional_phrase = do 
        prep <- pickRandom prepostions
        phrase <- noun_phrase
        return (prep ++ " " ++ phrase)
    
    verb_phrase  :: IO String
    verb_phrase = do 
        verb <- pickRandom verbs
        nphrase <- noun_phrase
        pphrase <- prepositional_phrase
        return (verb ++ " " ++ nphrase ++ " " ++ pphrase)


    sentence :: IO String
    sentence = do 
        nphrase <- noun_phrase
        vphrase <- verb_phrase
        prob <- probability 0.25
        if prob then do
            conj <- pickRandom conjunctions
            sent <- sentence
            return (nphrase ++ " " ++ vphrase ++ " " ++ conj ++ " " ++ sent)
        else do 
            return (nphrase ++ " " ++ vphrase)
