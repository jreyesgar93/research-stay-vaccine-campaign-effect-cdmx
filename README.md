# research-stay-vaccine-campaign-effect-cdmx


####  Clone the repository, update pip and generate a pyenv and renv. 

- Clone the followinr repositpy:

`git clone https://github.com/jreyesgar93/research-stay-vaccine-campaign-effect-cdmx.git`

- Actualizas el pip install:

`pip install --upgrade pip`

- Create ([a virtual environment](https://github.com/pyenv/pyenv)), and activate pyenv:

```
pyenv virtualenv covid-vaccine-effect
pyenv activate covid-vaccine-effect
```

- Make sure to install the following packages: 

```
sudo apt install unzip
sudo apt install curl
```

#### Install packages 
Install the following python packages: 

```
pip install -r requirements.txt
```


#### Download Data  
Run the following command to download the data 

```
./download_data.sh
```