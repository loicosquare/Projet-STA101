import pandas as pd
from sklearn.model_selection import train_test_split
import os

# Obtenir le répertoire courant
current_directory = os.getcwd()

# Spécifier le chemin relatif vers le fichier 'data.csv' dans le sous-dossier 'data'
file_path = os.path.join(current_directory, 'data', 'bdd.csv')

#Afficher file_path
print(file_path)

# Lire le fichier CSV avec pandas
data = pd.read_csv(file_path)

# Taille de l'échantillon final
sample_size = 200

# Stratified sampling
stratified_sample, _ = train_test_split(
    data,
    train_size=sample_size / len(data),  # Proportion de l'échantillon
    stratify=data['target'],  # Variable pour stratification
    random_state=42
)

# Vérifier les proportions dans l'échantillon
print("Proportions dans l'échantillon :")
print(stratified_sample['target'].value_counts(normalize=True))

print("Proportions dans les données d'origine :")
print(data['target'].value_counts(normalize=True))

# Sauvegarder l'échantillon si nécessaire
stratified_sample.to_csv('stratified_sample_bdd.csv', index=False)
