# Project: Weakly supervised learning-- label noise and correction


### [Full Project Description](doc/project3_desc.md)

+ Team members
	+ Ayati-Ghaffari, Arya (aa4663)
	+ Cook, Kerry (ksc2138)
	+ Sun, Jialiang (js5951)
	+ Zhang, Qian (qz2416)
	+ Zou, Yixuan (yz4004)

+ Project summary: In this project, we created a model I and model II. For model I, we used a basic CNN structure: two 2D convolutional layers, a max pooling layer, a flatten layer, a dense layer and the classficication layer. For model II, we trained a label cleaning network that follows a similar architecture as the paper suggests. We used a pre-trained CNN (VGG16) for the base network, and tried to match the rest of the architecture to the paper. The test accuracy on clean labels of model I and model II is around 51% and 58%, respectively.
	
```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
