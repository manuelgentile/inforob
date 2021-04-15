(clear-all) 

(defparameter *start-time* 0)


(defstruct need name expected_value current_value current_x status isNeed)


(defun log_to_file (step)
    (with-open-file (str "out.csv"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)

        (let ()
            (loop for dato in step do
                (format str "~A;" dato )
            )
            (format str "~%")
        )
    )
)



(defun logistic (x)
    (if x (/ (exp x) (+ 1 (exp x))) nil)
)

(defun replace-nth-from-list  (list n elem)  
      (cond  
        ((null list) ())  
        (t (append (subseq list 0 n) elem (subseq list (+ 1 n)(length list))))))

(defun update_need_satisfied (need)
    (let ()
        (setf (need-status need)  
            (if (< (abs (- (need-expected_value need) (need-current_value need))) 0.3) 'satisfied 'unsatisfied)
        )
    )
    
)

(defun update_needs(needs competence effect isNeed)
    (let ((updated_need nil)
          (flag nil)
          
        )
        (loop for need in needs do
            (if (equal (need-name need) competence)
                (if (equal effect 'unknown)
                    (let ()
                        (setf flag T)
                        (setf (need-current_x need) 'unknown)
                        (setf (need-current_value need) 'unknown)
                        (setf (need-status need) 'unsatisfied)
                        (log_to_file (list (- (mp-time-ms) *start-time*) "update_need" competence 'unknown 'unknown 'unsatisfied))
                        (if updated_need (setq updated_need (push need updated_need)) (setq updated_need (list need)))
                    )

                    (let* (
                            (valore_x_attuale 
                                    (if (equal (need-current_x need) 'unknown) 
                                        effect  
                                        (+ (need-current_x need) effect)
                                    )
                            )
                            (valore_y_attuale (if valore_x_attuale (logistic valore_x_attuale) nil))
                            (satisfied (if valore_y_attuale 
                                    (if (< (abs (- (need-expected_value need) valore_y_attuale)) 0.3)
                                'satisfied 'unsatisfied) 'unsatisfied))
                            
                        )
                        (setf flag T)
                        (setf (need-current_x need) valore_x_attuale)
                        (setf (need-current_value need) valore_y_attuale)
                        (setf (need-status need) satisfied)
                        (log_to_file (list (- (mp-time-ms) *start-time*) "update_need" competence valore_x_attuale valore_y_attuale satisfied))
                        (if updated_need (setq updated_need (push need updated_need)) (setq updated_need (list need)))
                    )
                )
                (if updated_need (setq updated_need (push need updated_need)) (setq updated_need (list need)))
            )
        )
        (if (not flag)
            (let ((nuova_need nil))
                (if (equal effect 'unknown)
                    (setf nuova_need (make-need :name competence :expected_value 1 :current_value 'unknown :current_x 'unknown :status 'unsatisfied :isNeed isNeed))
                    (let* (
                            (valore_x_attuale effect)
                            (valore_y_attuale (if valore_x_attuale (logistic valore_x_attuale) nil))
                            (satisfied (if valore_y_attuale (if (< (abs (- 1 valore_y_attuale)) 0.3)
                            'satisfied 'unsatisfied) 'unsatisfied))

                        )
                        (setf nuova_need (make-need :name competence :expected_value 1 :current_value valore_y_attuale :current_x valore_x_attuale :status satisfied :isNeed isNeed))
                        (log_to_file (list (- (mp-time-ms) *start-time*) "new_need" competence valore_x_attuale valore_y_attuale satisfied))
                    )
                )
                (if updated_need (setq updated_need (push nuova_need updated_need)) (setq updated_need (list nuova_need)))
            )

        )
        updated_need
    )
)  

(defun exist_unsatisfied_need (needs)    
    (let ()
    (loop for need in needs do
        (if (need-isNeed need)
            (if (equal (need-status need) 'unsatisfied) 
                (return-from exist_unsatisfied_need T)
            )
        )
    )
    nil
    )
)


(defun update_need_state (need actual_value expected_value)
    (let ((result nil))
        (cond 
            ((equal actual_value 'unknown) (setq result  (concatenate 'string (string need) "_NIL" )))
            ((> (- expected_value actual_value) 0.7) (setq result  (concatenate 'string (string need) "_LOW" )))
            ((> (- expected_value actual_value) 0.3) (setq result (concatenate 'string (string need) "_MID" )))
            (T (setq result (concatenate 'string (string need) "_HIGH" )))
        )
        (print result)
        (if result (find-symbol result))
    )       
)

(define-model persuasive

    (sgp :v t :act nil :esc t :lf .63 :mas 10 :ga 1.0 :imaginal-activation 1.0 :trace-detail high 
        :declarative-finst-span 6 :declarative-num-finsts 10)
    (sgp :style-warnings nil)

    (define-chunks introduction rules contagion mask wash_hands social_distancing vaccine conclusion)
    (define-chunks social_affiliation competence user_competence intentional_assessment open_mindness climax user_intention argumentation)

    (define-chunks unsatisfied satisfied)
    (define-chunks si no unknown)  
    (define-chunks need0 need1 need2 need3 need4 need5 need6)

    (chunk-type need_state (class c_need_state))
    (chunk-type dialogue_act name (class c_dialogue_act))
    (chunk-type scenes_dependency scene parent (class c_scenes_dependency))
    (chunk-type need_in_scene need scene default (class c_need_in_scene))

    (chunk-type action id scene needs dialogue_act (class c_action))
    (chunk-type action_effect action need effect (class c_action_effect))
    (chunk-type user_action_effect previous_dialogue_act_agent response need effect (isNeed nil) (class c_user_action_effect)) 
    
    (add-dm
        (argumentation_high isa need_state)
        (argumentation_mid isa need_state)
        (argumentation_low isa need_state)
        (argumentation_nil isa need_state)

        (user_competence_high isa need_state)
        (user_competence_mid isa need_state)
        (user_competence_low isa need_state)
        (user_competence_nil isa need_state)


        (user_intention_high isa need_state)
        (user_intention_mid isa need_state)
        (user_intention_low isa need_state)
        (user_intention_nil isa need_state)

        (role_intention_high isa need_state)
        (role_intention_mid isa need_state)
        (role_intention_low isa need_state)
        (role_intention_nil isa need_state)


        (competence_high isa need_state)
        (competence_mid isa need_state)
        (competence_low isa need_state)
        (competence_nil isa need_state)
        (intentional_assessment_high isa need_state)
        (intentional_assessment_mid isa need_state)
        (intentional_assessment_low isa need_state)
        (intentional_assessment_nil isa need_state)

        (graded_intentional_assessment_high isa need_state)
        (graded_intentional_assessment_mid isa need_state)
        (graded_intentional_assessment_low isa need_state)
        (graded_intentional_assessment_nil isa need_state)

        (social_affiliation_high isa need_state)
        (social_affiliation_mid isa need_state)
        (social_affiliation_low isa need_state)
        (social_affiliation_nil isa need_state)
        (open_mindness_high isa need_state)
        (open_mindness_mid isa need_state)
        (open_mindness_low isa need_state)
        (open_mindness_nil isa need_state)
        (climax_high isa need_state)
        (climax_mid isa need_state)
        (climax_low isa need_state)
        (climax_nil isa need_state)  

        (argument isa dialogue_act name argument)
        (exception1 isa dialogue_act name exception1)
        (exception2 isa dialogue_act name exception2)
        (final_greetings isa dialogue_act name final_greetings)
        (reinforce isa dialogue_act name reinforce)
        (inform isa dialogue_act name inform)
        (greetings isa dialogue_act name greetings)
        (question_knowledge isa dialogue_act name question_knowledge)
        (question_intention isa dialogue_act name question_intention)
        (question_intention_role isa dialogue_act name question_intention_role)
        (substitution isa dialogue_act name substitution)
        (graded_question_intention isa dialogue_act name graded_question_intention)

        (scenes_dependency1 isa scenes_dependency scene introduction parent noscene class c_scenes_dependency) 
        (scenes_dependency2 isa scenes_dependency scene rules parent introduction class c_scenes_dependency) 
        (scenes_dependency3 isa scenes_dependency scene contagion parent rules class c_scenes_dependency) 
        (scenes_dependency4 isa scenes_dependency scene mask parent contagion class c_scenes_dependency) 
        
        (scenes_dependency7 isa scenes_dependency scene vaccine parent mask class c_scenes_dependency) 

        (scenes_dependency10 isa scenes_dependency scene conclusion parent vaccine class c_scenes_dependency)  

        (need_in_scene1 isa need_in_scene need social_affiliation scene introduction class c_need_in_scene)          
        
        (need_in_scene2 isa need_in_scene need competence scene rules class c_need_in_scene)
        (need_in_scene3 isa need_in_scene need user_competence scene rules class c_need_in_scene)
        (need_in_scene4 isa need_in_scene need intentional_assessment scene rules class c_need_in_scene) 

        (need_in_scene5 isa need_in_scene need competence scene contagion class c_need_in_scene) 
        (need_in_scene6 isa need_in_scene need user_competence scene contagion class c_need_in_scene) 

        (need_in_scene7 isa need_in_scene need user_competence scene mask class c_need_in_scene) 
        (need_in_scene8 isa need_in_scene need intentional_assessment scene mask class c_need_in_scene) 
        (need_in_scene9 isa need_in_scene need climax scene mask class c_need_in_scene)     
             
        (need_in_scene20 isa need_in_scene need intentional_assessment scene vaccine class c_need_in_scene) 
        (need_in_scene21 isa need_in_scene need graded_intentional_assessment scene vaccine class c_need_in_scene) 

        (need_in_scene22 isa need_in_scene need social_affiliation scene conclusion class c_need_in_scene) 



        (act01 isa action id act01 scene introduction needs social_affiliation dialogue_act greetings class c_action) 
        (act02 isa action id act02 scene rules needs competence dialogue_act question_knowledge class c_action) 
        (act03 isa action id act03 scene rules needs competence dialogue_act inform class c_action) 
        (act04 isa action id act04 scene rules needs competence dialogue_act reinforce class c_action) 
        (act05 isa action id act05 scene rules needs intentional_assessment dialogue_act question_intention class c_action) 
        (act06 isa action id act06 scene contagion needs competence dialogue_act question_knowledge class c_action) 
        (act07 isa action id act07 scene contagion needs competence dialogue_act inform class c_action) 
        (act08 isa action id act08 scene contagion needs competence dialogue_act reinforce class c_action) 
        (act09 isa action id act09 scene mask needs competence dialogue_act inform class c_action) 
        (act10 isa action id act10 scene mask needs competence dialogue_act argument class c_action) 
        (act11 isa action id act11 scene mask needs competence dialogue_act argument class c_action) 
        (act12 isa action id act12 scene mask needs competence dialogue_act argument class c_action) 
        (act13 isa action id act13 scene mask needs climax dialogue_act exception1 class c_action) 
        (act14 isa action id act14 scene mask needs climax dialogue_act exception2 class c_action) 
        (act15 isa action id act15 scene mask needs intentional_assessment dialogue_act question_intention class c_action) 
        (act16 isa action id act16 scene mask needs intentional_assessment dialogue_act question_intention_role class c_action) 
        (act17 isa action id act17 scene mask needs open_mindness dialogue_act substitution class c_action) 
        
        (act19 isa action id act19 scene vaccine needs intentional_assessment dialogue_act question_intention class c_action) 
        
        (act20 isa action id act20 scene vaccine needs competence dialogue_act argument class c_action)
        (act20b isa action id act20b scene vaccine needs competence dialogue_act argument class c_action)
        (act20c isa action id act20c scene vaccine needs competence dialogue_act argument class c_action)


        (act21 isa action id act21 scene vaccine needs intentional_assessment dialogue_act graded_question_intention class c_action) 
        (act22 isa action id act22 scene conclusion needs social_affiliation dialogue_act greetings class c_action) 

        

        (action_effect1 isa action_effect action act01 need social_affiliation effect 1 class c_action_effect)
        (action_effect30 isa action_effect action act30 need social_affiliation effect 1 class c_action_effect)

        (action_effect2 isa action_effect action act03 need user_competence effect 2 class c_action_effect) 
        (action_effect3 isa action_effect action act04 need user_competence effect 0.5 class c_action_effect) 
        (action_effect3_0 isa action_effect action act05 need intentional_assessment effect 1 class c_action_effect) 

        (action_effect4 isa action_effect action act07 need user_competence effect 2 class c_action_effect) 
        (action_effect5 isa action_effect action act08 need user_competence effect 0.5 class c_action_effect) 


        (action_effect6 isa action_effect action act09 need user_competence effect 1 class c_action_effect) 

        (action_effect7 isa action_effect action act10 need argumentation effect 1 class c_action_effect) 
        (action_effect8 isa action_effect action act11 need argumentation effect 1 class c_action_effect) 
        (action_effect9 isa action_effect action act12 need argumentation effect 1 class c_action_effect) 

        (action_effect7b isa action_effect action act10 need user_competence effect 1 class c_action_effect) 
        (action_effect8b isa action_effect action act11 need user_competence effect 1 class c_action_effect) 
        (action_effect9b isa action_effect action act12 need user_competence effect 1 class c_action_effect)


        (action_effect10 isa action_effect action act13 need climax effect 1 class c_action_effect) 
        (action_effect11 isa action_effect action act14 need climax effect 1 class c_action_effect) 

        (action_effect10b isa action_effect action act13 need role_intention effect unknown class c_action_effect) 
        (action_effect11b isa action_effect action act14 need role_intention effect unknown class c_action_effect) 

        (action_effect10a isa action_effect action act13 need intentional_assessment effect unknown class c_action_effect) 
        (action_effect11a isa action_effect action act14 need intentional_assessment effect unknown class c_action_effect) 

        (action_effect12 isa action_effect action act17 need open_mindness effect 2 class c_action_effect) 
        (action_effect13 isa action_effect action act18 need competence effect 1 class c_action_effect) 
        (action_effect14 isa action_effect action act20 need competence effect 1 class c_action_effect) 
        (action_effect15 isa action_effect action act22 need social_affiliation effect 1 class c_action_effect)  

        (action_effect16 isa action_effect action act21 need graded_intentional_assessment effect 1 class c_action_effect)  


        (action_effect20 isa action_effect action act20 need argumentation effect 1 class c_action_effect) 
        (action_effect20b isa action_effect action act20b need argumentation effect 1 class c_action_effect) 
        (action_effect20c isa action_effect action act20c need argumentation effect 1 class c_action_effect) 

        (user_action_effect1 isa user_action_effect previous_dialogue_act_agent question_knowledge response si need competence effect 1 class c_user_action_effect) 
        (user_action_effect2 isa user_action_effect previous_dialogue_act_agent question_knowledge response no need competence effect 1 class c_user_action_effect) 

        (user_action_effect5 isa user_action_effect previous_dialogue_act_agent question_knowledge response si need user_competence effect 0.5 class c_user_action_effect) 
        (user_action_effect6 isa user_action_effect previous_dialogue_act_agent question_knowledge response no need user_competence effect -1 class c_user_action_effect) 

        (user_action_effect3 isa user_action_effect previous_dialogue_act_agent question_intention response si need user_intention effect 1 class c_user_action_effect) 
        (user_action_effect4 isa user_action_effect previous_dialogue_act_agent question_intention response no need user_intention effect -1 class c_user_action_effect)
        (user_action_effect9 isa user_action_effect previous_dialogue_act_agent question_intention response no need argumentation effect unknown class c_user_action_effect)
        


        (user_action_effect3b isa user_action_effect previous_dialogue_act_agent question_intention_role response si need role_intention effect 1 class c_user_action_effect) 
        (user_action_effect4b isa user_action_effect previous_dialogue_act_agent question_intention_role response no need role_intention effect -1 class c_user_action_effect)
        (user_action_effect10b isa user_action_effect previous_dialogue_act_agent question_intention_role response no need open_mindness effect -1 class c_user_action_effect)

        (user_action_effect7 isa user_action_effect previous_dialogue_act_agent question_intention response si need intentional_assessment effect 1 class c_user_action_effect) 
        (user_action_effect8 isa user_action_effect previous_dialogue_act_agent question_intention response no need intentional_assessment effect 1 class c_user_action_effect)
        (user_action_effect7b isa user_action_effect previous_dialogue_act_agent question_intention_role response si need intentional_assessment effect 1 class c_user_action_effect) 
        (user_action_effect8b isa user_action_effect previous_dialogue_act_agent question_intention_role response no need intentional_assessment effect 1 class c_user_action_effect)

    )


    (chunk-type information_state   				
                    
                    state
    				current_scene previous_scene 
                    current_action
                    current_dialogue_act
                    current_dialogue_act_agent
                    previous_dialogue_act_agent
                    previous_found_dialogue_act_agent
                    user_response
                    need0
                    need1
                    need2 
                    need3 
                    need4
                    need5
                    need6 
                    vars_list
                    index_need
                    max_index_need
    				(class c_information_state))

    
    (p create_information_state
    	=goal>
            ISA information_state
            state init
        ==>
        !eval! (setf *start-time* (mp-time-ms))
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "start"))
        =goal>
            ISA information_state
            state scene_selection
    	    previous_scene noscene
        )

    (p search_scene 
        =goal>
            ISA information_state
            state scene_selection
            previous_scene =last_scene
        ?retrieval>
            state free
            buffer empty
        ==>
        +retrieval>
            ISA scenes_dependency
            parent =last_scene
        )

    (p found_scene
        =goal>
            ISA information_state
            state scene_selection
        =retrieval>
            ISA scenes_dependency
            scene =scene
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "scene_change" =scene))
        !output! =scene 
        !output! "Clean list"          
        =goal>
            ISA information_state
            state remove_needs_slots
            current_scene =scene
            previous_scene nil
            vars_list nil
            index_need 0
        )

    (p prova2
        =goal>
            ISA information_state
            state remove_needs_slots        
            index_need =index_need
        ==>
        !output! "first scene"
        =goal>
            ISA information_state
            state needs_in_scene_retrieval       
    )

    (p remove_needs_slots
        =goal>
            ISA information_state
            state remove_needs_slots
            max_index_need =max_index_need
            index_need =index_need
        !eval! (< =index_need =max_index_need)
        ==>
        !bind! =need_slot (find-symbol (concatenate 'string "NEED" (write-to-string =index_need)))
        !bind! =new_index_need (+ 1 =index_need)
        !output! "Clean need slot"
        !output! =index_need
        =goal>
            ISA information_state
            =need_slot nil
            index_need =new_index_need         
    )

    (p no_more_remove_needs_slots
        =goal>
            ISA information_state
            state remove_needs_slots
            max_index_need =max_index_need
            index_need =index_need
        !eval! (not (< =index_need =max_index_need))
        ==>
        !output! "max index need"
        !output! =max_index_need
        =goal>
            ISA information_state
            state needs_in_scene_retrieval
            index_need 0     
    )

    

    (p search_need_in_scene
        =goal>
            ISA information_state
            state needs_in_scene_retrieval
            current_scene =scene
        ?retrieval>
            state free
            buffer empty
        ==>
    	+retrieval>
    		ISA need_in_scene
    		scene =scene
            :recently-retrieved nil
        )

    (p found_need_in_scene_no_list
        =goal>
            ISA information_state
            state needs_in_scene_retrieval
        =retrieval>
            ISA need_in_scene
            scene =scene
            need =need
        ==>
        !bind! =need_s (make-need :name =need :expected_value 1 :current_value 'unknown :current_x 'unknown :status 'unsatisfied :isNeed T)
        !bind! =new_list (cons =need_s nil)
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "needs_add_in_scene" =scene =need))
        =goal>
            vars_list =new_list
        )

    (p found_need_in_scene
        =goal>
            ISA information_state
            state needs_in_scene_retrieval
            vars_list =vars_list
        =retrieval>
            ISA need_in_scene
            scene =scene
            need =need
        ==>
        !bind! =need_s (make-need :name =need :expected_value 1 :current_value 'unknown  :current_x 'unknown :status 'unsatisfied :isNeed T)
        !bind! =new_vars_list (cons =need_s =vars_list)
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "needs_add_in_scene" =scene =need))
        =goal>
            vars_list =new_vars_list
        )


    (p found_need_in_scene_no_vars_list_default
        =goal>
            ISA information_state
            state needs_in_scene_retrieval
        =retrieval>
            ISA need_in_scene
            scene =scene
            need =need
            default =default
        ==>

        !bind! =need_s (make-need :name =need :expected_value 1 :current_value (logistic =default) :current_x =default :isNeed T)
        !eval! (update_need_satisfied =need_s)
        !bind! =new_vars_list (cons =need_s nil)
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "needs_add_in_scene_with_default" =scene =need))
        =goal>
            vars_list =new_vars_list
        )

    (p found_need_in_scene_default
        =goal>
            ISA information_state
            state needs_in_scene_retrieval
            vars_list =vars_list
        =retrieval>
            ISA need_in_scene
            scene =scene
            need =need
            default =default
        ==>
        !bind! =need_s (make-need :name =need :expected_value 1 :current_value (logistic =default)  :current_x =default :isNeed T)
        !eval! (update_need_satisfied =need_s)
        !bind! =new_vars_list (cons =need_s =vars_list)
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "needs_add_in_scene_with_default" =scene =need))
        =goal>
            vars_list =new_vars_list
        )

    (p no_more_needs_in_scene
        =goal>
            ISA information_state
            state needs_in_scene_retrieval
            vars_list =vars_list
        ?retrieval>
            buffer failure    
        ==>
        !bind! =max_index_need (list-length =vars_list)
        =goal>
            ISA information_state
            state update_information_state_needs
            index_need 0
            max_index_need =max_index_need
        )

    (p update_needs_slots
        =goal>
            ISA information_state
            state update_information_state_needs
            index_need =index_need
            max_index_need =max_index_need
            vars_list =vars_list
        !eval! (< =index_need =max_index_need)
        ==>
        !bind! =need_s (nth =index_need =vars_list)
        !bind! =need (need-name =need_s)
        !bind! =expected_value (need-expected_value =need_s)
        !bind! =actual_value (need-current_value =need_s)
        !bind! =need_state (update_need_state =need =actual_value =expected_value)

        !bind! =need_slot (find-symbol (concatenate 'string "NEED" (write-to-string =index_need)))
        !bind! =new_index_need (+ 1 =index_need)
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "needs_update_slot" =need_slot =need_state))
        =goal>
            ISA information_state
            state update_information_state_needs
            =need_slot =need_state
            index_need =new_index_need 
        )

    
    (p no_more_need_slot_to_update
        =goal>
            ISA information_state
            state update_information_state_needs
            index_need =index_need
            max_index_need =max_index_need
        !eval! (not (< =index_need =max_index_need))
        ==>
        !output! "azzera index_need dopo aggiornamento valori"    
        =goal>
            ISA information_state
            state next
            index_need 0
        )

    (p found_unsatisfied_needs
        =goal>
            ISA information_state
            state next
            vars_list =vars_list
        !eval! (exist_unsatisfied_need =vars_list)
        ==>
        =goal>
            ISA information_state
            state select_dialogue_act
        )

    (p no_more_unsatisfied_needs
        =goal>
            ISA information_state
            state next
            current_scene =current_scene
            vars_list =vars_list
        !eval! (not (exist_unsatisfied_need =vars_list))
        ==>
        =goal>
            ISA information_state
            state scene_selection
            previous_scene =current_scene
            current_scene nil
        )


    (p find_dialogue_act
        =goal>
            ISA information_state
            state select_dialogue_act
            current_scene =scene
        ?retrieval>
            state free
            buffer empty
        ==>
        +retrieval>
            ISA dialogue_act
        )

    (p find_dialogue_act_with_previous_d_act
        =goal>
            ISA information_state
            state select_dialogue_act
            current_scene =scene
            previous_found_dialogue_act_agent =dialogue_act
        ?retrieval>
            state free
            buffer empty
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "not found action with dialogue_act" =dialogue_act) )
        +retrieval>
            ISA dialogue_act
            - name =dialogue_act
        )

    (p found_dialogue_act
        =goal>
            ISA information_state
            state select_dialogue_act
            current_scene =scene
        =retrieval>
            ISA dialogue_act
            name =name
        ==>
        !output! "Dialogue ACT"
        !output! =name
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "dialogue_act_found" =name) )
        =goal>
            ISA information_state
            state select_action
            current_dialogue_act =name
            previous_found_dialogue_act_agent nil
        )

    (p find_action
        =goal>
            ISA information_state
            state select_action
            current_scene =scene
            current_dialogue_act =name
        ?retrieval>
            state free
            buffer empty
        ==>
        +retrieval>
            ISA action
            scene =scene
            dialogue_act =name
            :recently-retrieved nil
        )

    (p no_action_found
        =goal>
            ISA information_state
            state select_action
            current_scene =scene
            current_dialogue_act =dialogue_act
        ?retrieval>
            buffer failure
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "agent_action_not_found" =dialogue_act) )          
        =goal>
            ISA information_state
            state select_dialogue_act
            previous_found_dialogue_act_agent =dialogue_act
            current_dialogue_act nil
        )

    (p action_found
        =goal>
            ISA information_state
            state select_action
            current_scene =scene
        =retrieval>
            ISA action
            scene =scene
            id =id
            dialogue_act =dialogue_act  
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "agent_action_found" =id) )          
        =goal>
            ISA information_state
            state running
            current_scene =scene
            current_action =id
            previous_dialogue_act_agent =dialogue_act
        )
    
    (p execute_action
        =goal>
            ISA information_state
            state running
            current_action =id
            current_scene =scene
        ?vocal>
            state free
        ==>
        =goal>
            ISA information_state
            state search_effects
            current_dialogue_act nil
        !bind! =str_action (string =id)
        !output! =str_action
        +vocal>
            cmd speak
            string =str_action
        )

    (p search_action_effect
        =goal>
            ISA information_state
            state search_effects
            current_action =id
        ?retrieval>
            state free
        ==>
        +retrieval>
            ISA action_effect
            action =id
            :recently-retrieved nil
        )

    (p found_and_apply_action_effect
        =goal>
            ISA information_state
            state search_effects
            vars_list =vars_list
        =retrieval>
            ISA action_effect
            need =need
            effect =effect          
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "agent_action_effect" =need =effect) )       
        !bind! =new_vars_list (update_needs =vars_list =need =effect nil)
        !bind! =max_index_need (list-length =new_vars_list)        
        =goal>
            ISA information_state
            vars_list =new_vars_list
            max_index_need =max_index_need
        )

    (p no_action_effect
        =goal>
            ISA information_state
            state search_effects
        ?retrieval>
            buffer failure
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "agent_action_no_more_effect"))
        =goal>
            ISA information_state
            state update_information_state_needs2
            index_need 0
        )


    (p update_needs_slots2
        =goal>
            ISA information_state
            state update_information_state_needs2
            index_need =index_need
            max_index_need =max_index_need
            vars_list =vars_list
        !eval! (< =index_need =max_index_need)
        ==>
        !bind! =need_s (nth =index_need =vars_list)
        !output! "update need slot dopo agent action"
        !output! =need_s
        !bind! =need (need-name =need_s)
        !bind! =expected_value (need-expected_value =need_s)
        !bind! =actual_value (need-current_value =need_s)
        !bind! =need_state (update_need_state =need =actual_value =expected_value)
        !bind! =need_slot (find-symbol (concatenate 'string "NEED" (write-to-string =index_need)))
        !bind! =new_index_need (+ 1 =index_need)
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "needs_update_state" =need_slot =need_state))
        =goal>
            ISA information_state
            state update_information_state_needs2
            =need_slot =need_state
            index_need =new_index_need 
        )

    
    (p no_more_need_slot_to_update2
        =goal>
            ISA information_state
            state update_information_state_needs2
            index_need =index_need
            max_index_need =max_index_need
        !eval! (>= =index_need =max_index_need)
        ==>
        !output! "reset index_need after values update"    
        =goal>
            ISA information_state
            state auto_ascolto
            index_need 0
        )


    (p consume_auto_input
        =goal>
            ISA information_state
            state auto_ascolto
        =aural-location>
            isa      audio-event
            location =who
        ?aural>
            state    free
        !eval! (string= "SELF" (symbol-name =who))
        ==>
        !eval! (print =who)
        !eval! (print (symbolp =who))
        !eval! (print (string= "SELF" (symbol-name =who)))
        +aural>
            isa      sound
            event    =aural-location
        =goal>
            ISA information_state
            state wait_user_input
        
    )

    (p wait_user_aural
        =goal>
            ISA information_state
            state wait_user_input
        =aural-location>
            isa      audio-event
            location =who
        ?aural>
            state    free
        !eval! (string= "EXTERNAL" (symbol-name =who))
        ==>
        !eval! (print =who)
        !eval! (print "ciao")
        !eval! (print (string= "EXTERNAL" (symbol-name =who)))
        +aural>
            isa      sound
            event    =aural-location
        =goal>
            ISA information_state
            state interpreting_user_input
    )
    

    (p listening-comprehension
        =goal>
            ISA information_state
            state interpreting_user_input
        =aural>
            isa     sound
            content =content
        ?retrieval>
            state   free
        ==>
        !output! (=content)
        !bind! =content_symbol (find-symbol =content)
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "user_action" =content))
        =goal>
            ISA information_state
            state search_user_effects
            user_response =content_symbol
     )

    (p search_user_action_effect
        =goal>
            ISA information_state
            state search_user_effects
            previous_dialogue_act_agent =previous_dialogue_act
            user_response =user_response
        ?retrieval>
            state free
        ==>
        +retrieval>
            ISA user_action_effect
            previous_dialogue_act_agent =previous_dialogue_act
            response =user_response
            :recently-retrieved nil
        )

    (p found_and_apply_user_action_effect
        =goal>
            ISA information_state
            state search_user_effects
            vars_list =vars_list
        =retrieval>
            ISA user_action_effect
            need =need
            effect =effect          
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "user_action_effect" =need =effect) )
        !bind! =new_vars_list (update_needs =vars_list =need =effect nil)
        !bind! =max_index_need (list-length =new_vars_list)
        
        =goal>
            ISA information_state
            vars_list =new_vars_list
            max_index_need =max_index_need
        )

    (p found_and_apply_user_action_effect_isNeed
        =goal>
            ISA information_state
            state search_user_effects
            vars_list =vars_list
        =retrieval>
            ISA user_action_effect
            need =need
            effect =effect
            isNeed T         
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "user_action_effect" =need =effect) )
        !bind! =new_vars_list (update_needs =vars_list =need =effect T)
        !bind! =max_index_need (list-length =new_vars_list)
        
        =goal>
            ISA information_state
            vars_list =new_vars_list
            max_index_need =max_index_need
        )
    
    (p no_user_action_effect
        =goal>
            ISA information_state
            state search_user_effects
        ?retrieval>
            buffer failure
        ==>
        !eval! (log_to_file (list (- (mp-time-ms) *start-time*) "user_action_no_more_effect"))
        =goal>
            ISA information_state
            state update_information_state_needs
            previous_dialogue_act_agent nil
        )

    
    (add-sji
        (social_affiliation_nil greetings 5)
        
        (competence_nil question_knowledge 10)
        
        (user_competence_nil inform 9)
        (user_competence_low inform 8)
        (user_competence_mid reinforce 7)

        (intentional_assessment_nil question_intention 6)
        (intentional_assessment_mid question_intention 5)
        
        (intentional_assessment_nil question_intention_role 6)
        (intentional_assessment_mid question_intention_role  5)

        (graded_intentional_assessment_nil graded_question_intention 5)

        (user_intention_nil question_intention 1)
        (role_intention_nil question_intention_role 1)

        (argumentation_nil argument 10)

        (climax_nil exception1 5)
        (climax_nil exception2 5)

        (user_intention_mid exception1 5)
        (user_intention_low exception2 5)

        (open_mindness_mid substitution 10)
        (open_mindness_low substitution 10)   
    )
)
