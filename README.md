# InfoRob

This repository contains the ACT-R model of a storytelling robot, that is able to adopt different kinds of persuasive techniques and ethical stances while conversing about some topics related to COVID-19.  

The model is described in the paper  "A Storytelling Robot managing Persuasive and Ethical Stances via ACT-R: An Exploratory Study", International Journal of Social Robotics https://link.springer.com/article/10.1007/s12369-021-00847-w. Also available at https://arxiv.org/pdf/2107.12845.pdf

The main contribution of the paper consists in the proposal of a needs-driven model that guides and evaluate, during the dialogue, the overall decision making strategies of an agent about the appropriateness of activating (or not) some persuasive techniques available in the agent procedural memory of the ACT-R architecture. 
The portfolio of persuasive techniques tested in such a model range from the use of storytelling to framing techniques and fallacious-based arguments. To the best of our knowledge this represents the first attempt of building a persuasive agent by making use of an integrated mix of explicitly grounded cognitive assumptions about dialogue management, storytelling and persuasive techniques.

# How to run the model

To run the InfoRob ACT-R model (inforob_model.lisp):
* put the inforob_model.lisp file in the directory $ACTR_HOME$/model/persuasive
* start the ACT-R software
* run the python client inforob.py (python inforob.py)

The current version of the dialogue is in the Italian language. The English version of the dialogue is available at https://github.com/manuelgentile/inforob/blob/main/inforob_en.py


