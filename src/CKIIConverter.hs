{-| LANGUAGE OverloadedStrings #-}
module CKIIConverter where

import           CKIIParser

--Shared types:
newtype Name = Name String
newtype BirthDate = BirthDate String
newtype Religion = Religion String
newtype Culture = Culture String

--CKII types:
newtype CKIICharacter = CKIICharacter Name BirthDate Religion Culture Attributes Traits
newtype Attributes = Attributes Integer Integer Integer Integer Integer deriving (Show) 
newtype Traits = [Integer]
--EU4 types:

--EU4 Countries have: a three letter tag, a bunch of ancillary details, and a ruler, and a heir.
newtype EU4Country = EU4Country Tag EU4Government EU4Ruler EU4Ruler  deriving (Show) --Tag
newtype Tag = Tag String
data EU4Government = monarchy | republic | tribal | native | theocracy
data EU4GovernmentReforms
newtype EU4Ruler = EU4Ruler Name BirthDate EU4Dynasty Stat Stat Stat [EU4Personality]
newtype EU4Dynasty = EU4Dynasty String
newtype Stat = Stat Integer
data EU4Personality = just_personality | righteous_personality | tolerant_personality |
                      kind_hearted_personality | free_thinker_personality | well_connected_personality |
                      calm_personality | careful_personality | secretive_personality | 
                      intricate_web_weaver_personality | fertile_personality | well_advised_personality |
                      benevolent_personality | zealot_personality  | pious_personality | 
                      lawgiver_personality | midas_touched_personality | incorruptible_personality |
                      architectural_visionary_personality | scholar_personality | entrepreneur_personality |
                      industrious_personality | expansionist_personality | charismatic_negotiator_personality |
                      silver_tongue_personality | conqueror_personality | tactical_genius_personality |
                      bold_fighter_personality | strict_personality | inspiring_leader_personality |
                      martial_educator_personality | navigator_personality | fierce_negotiator_personality |
                      babbling_buffoon_personality | embezzler_personality | infertile_personality |
                      drunkard_personality | sinner_personality | greedy_personality | cruel_personality |
                      naive_personality | craven_personality | loose_lips_personality |
                      obsessive_perfectionist_personality | malevolent_personality | immortal_personality deriving (Show)


--Conversion Functions
tuplify5 :: [a] -> (a,a,a,a,a)
tuplify5 [a,b,c,d,e] = (a,b,c,d,e)

readCharacter :: SaveFileMap -> CKIICharacter -- MaybeCKIICharacter?
readCharacter input = CKIICharacter (lookup "bn" input) (lookup "b_d" input) (lookup "rel" input) (lookup "cul" input) (tuplify5 (lookup "att" input)) (lookup "traits" input)