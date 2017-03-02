Feature: Word
    Level: Red
    Level: Blue
    Level: Green

Feature: Color
    Level: Red
    Level: Blue
    Level: Green

Constraint:
    No (Word:Red) Followed_By (Word:Blue)

Experiment:
    Fully_Cross(Word, Color)



-- to do: YAML
