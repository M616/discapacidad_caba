import pandas as pd
import gdown


file_id = "1ekHA_DuteVXaLznTBJEbktr-P_O3lxEF"
url = f"https://drive.google.com/uc?export=download&id={file_id}"

gdown.download(url, output="archivo.csv", quiet=False, fuzzy=True)
gdown.download(url, "data/DGINC-DA-BASE_Marzo - recibida el 19-05.csv", quiet=False, use_cookies=True)
df = pd.read_csv("archivo.csv")