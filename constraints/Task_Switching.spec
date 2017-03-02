Factor: Feature_Correlation
    Level: Correlated
    Level: Uncorrelated

Factor: Congruency
    Level: Incongruent
    Level: Congruent

Factor: Task_Transition
    Level: Same_Task
    Level: Different_Task

Factor: Task
    Level: Color Task
    Level: Motion_Task

Factor: Response
    Level: Left
    Level: Right

Factor: Response_Transition
    Level: Same_As_Next
    Level: Different_From_Next

Factor: Color
    Level: Blue
    Level: Red

Factor: Motion
    Level: Up
    Level: Down

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constraint:
    No (Word:Red) Followed_By (Word:Blue)
    Validate (Task:All) By (Task_Transition:All) # All means all levels
    Validate (Response:All) By (Response_Transition:All)
    Max_in_a_row (Task:All) 7
    Max_in_a_row (Response:All) 5

Experiment:
    Fully_Cross(Task, Response, Feature_Correlation,
                Task_Transition, Response_Transition, Congruency)
