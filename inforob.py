import actr

utterances= {
'act01':'Ciao, mi chiamo InfoRob, sono qui per darti suggerimenti in tema di salute e prevenzione sul COVID-19.',
'act02':'Come saprai ci sono alcune regole di base da seguire per evitare il diffondersi del contagio. Sai quali sono?',
'act03':'Le 3 regole principali anti-Covid sono: indossare la mascherina protettiva, lavare spesso le mani e rispettare il distanziamento sociale',
'act04':'Come sai, le 3 regole principali anti-Covid sono: indossare la mascherina protettiva, lavare spesso le mani e rispettare il distanziamento sociale',
'act05':'Sei d\'accordo sul fatto che queste misure siano sempre importanti da seguire e da applicare congiuntamente?',
'act06':'Conosci le principali cause di diffusione del virus?',
'act07':'Il Covid si diffonde piu\' facilmente per via aerea',
'act08':'Come sai, il Covid si diffonde piu\' facilmente per via aerea',
'act09':'Indossare una mascherina (DPI) e\' una misura di prevenzione in quanto limita la fuoriuscita di particelle di droplets e aerosol.',
'act10':'La grande maggioranza delle persone ritiene che indossare la mascherina ostacoli significativamente la diffusione del virus',
'act11':'Se non usi la mascherina il rischio di contagio aumenta dell\' 80%% rispetto a chi usa la mascherina e, in piu\', rischi di infettare anche i tuoi cari con conseguenze drammatiche ',
'act12':'La prevenzione e\' importante per arrestare o rallentare il contagio da COVID e, come dice Alberto Angela, e\' necessario usare la mascherina per prevenire il COVID. ',
'act13':'Ok, adesso supponi di essere una persona fortemente allergica al materiale di tutte le diverse tipologie di mascherina. ',
'act14':'Effettivamente ci sono dei casi in cui potrebbe essere un problema rispettare queste misure. Per esempio mettiti nei panni di una persona  fortemente allergica al materiale di tutte le diverse tipologie di mascherina.',
'act15':'Sei d\'accordo circa la necessita\' di dover indossare la mascherina?',
'act16':'Useresti la mascherina in questo caso?',
'act17':'Considera il fatto che, comunque, in caso di allergia alla mascherina puoi diminuire la possibilita\' di contagio seguendo le altre due regole virtuose, ovvero mantenendo la distanza e lavandoti spesso le mani.',
'act19':'Saresti favorevole a vaccinarti?',
'act20':'Piero Angela si e\' dichiarato disponibile a fare pubblicamente il vaccino dicendo che i vaccini sono sicuri ed efficaci',
'act20b':'La grande maggioranza delle persone ritiene che vaccinarsi sia l\'unica soluzione per uscire dall\'emergenza sanitaria',
'act20c':'Il 100%% dei vaccinati non sviluppa forme gravi della malattia (sia nella sua forma originaria che nella variante inglese)',
'act21':'Quanto sei disposto a vaccinarti su una scala da 1 a 10? (dove 1 sta per non cambio idea e 10 per cambio idea)',
'act22':'Grazie per la conversazione. Ti chiedo cortesemente di rispondere al seguente questionario'
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