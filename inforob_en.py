import actr

utterances= {
act01':'Hi, my name is InfoRob, I\'m here to give you health and prevention tips about COVID-19.',
act02':'As you know there are some basic rules to follow to avoid the spread of the infection. Do you know what they are?',
act03':'The 3 main anti-Covid rules are: wear a face mask, wash your hands often and respect social distancing',
act04':'As you know, the 3 main anti-Covid rules are: wear a face mask, wash your hands often and respect social distancing',
act05':'Do you agree that these measures are always important to follow and to apply together?',
act06':'Do you know the main causes of the spread of the virus?',
act07':'Covid is most easily spread by air',
act08:'As you know, Covid is most easily spread by air',
act09: 'Wearing a mask (PPE) is a preventive measure as it limits the release of droplets and aerosol particles',
act10':'The vast majority of people believe that wearing a mask significantly hinders the spread of the virus',
act11':'If you don\'t wear a mask, your risk of infection increases by 80% compared to those who wear a mask and, in addition, you risk infecting your loved ones with dramatic consequences',
act12':'Prevention is important to stop or slow down COVID infection and, as Alberto Angela says, it is necessary to use a mask to prevent COVID.',
act13':'Ok, now suppose you are a person who is highly allergic to the material of all the different types of masks.',
act14':'Actually there are cases where it might be a problem to comply with these measures. For example, put yourself in the position of a person who is highly allergic to the material of all the different types of mask',
act15':'Do you agree that it is necessary to wear the mask?',
act16':'Would you use the mask in this case?',
act17':'Consider the fact that, however, if you are allergic to the mask, you can reduce the chance of contagion by following the other two good rules, i.e. keeping your distance and washing your hands often',
act19':'Would you be in favour of vaccination?',
act20':'Piero Angela has declared his willingness to vaccinate publicly saying that vaccines are safe and effective',
act20b':'The vast majority of people believe that getting vaccinated is the only way out of the health emergency',
act20c':'100%% of those vaccinated do not develop severe forms of the disease (either in its original form or in the English variant)',
act21':'How willing are you to vaccinate on a scale of 1 to 10? (where 1 is for not changing my mind and 10 is for changing my mind)',
act22':'Thank you for the conversation. I kindly ask you to answer the following questionnaire.'
}

def process_moves(model, string):
	print(string)
	print(utterances.get(string.lower()))

def main():
	actr.reset()	
	actr.load_act_r_model ("ACT-R:models;persuasive;inforob_model.lisp")
	actr.hide_output()
	actr.install_device(["speech","microphone"])
	actr.add_command("process-moves", process_moves, "Handle player speak actions")
	actr.monitor_command("output-speech","process-moves")
	actr.set_current_model("persuasive")
	#(define-chunks (g1 isa information_state state init))
	actr.goal_focus(actr.define_chunks(['isa', 'information_state', 'state','init'])[0])
	#(need_in_scene10 isa need_in_scene need open_mindness scene mask default 1 class c_need_in_scene)	
	actr.sdp('ACT11',":base-level",1)
	actr.add_dm(['isa', 'need_in_scene', 'need','open_mindness','scene','mask','default',1])

	while True :
		actr.run(20)
		r=raw_input(">")
		if r=="":
			break
		#r="\""+r+"\""

		actr.new_word_sound(r)

	actr.remove_command_monitor("output-speech", "process-moves")
	actr.remove_command("process-moves")
	print ("End")



if __name__ == "__main__":  
    main()
