# SaudiFootballClubsProject
By analyzing multi-season data, this project uncovers performance patterns and data-driven strategies for Saudi clubs.

### Datasets
Football (Soccer) data for Saudi Professional League Since 2000.
- All files are .csv
- File name meaning (e.g. SPL-YEAR-YEAR-DATASOURCE )
- Each file will have the following columns:
| MatchNo | Date | Time | Team1 | Team2 | Score1 | Score2 | Note |
| ------ | ------ | ------ | ------ | ------ | ------ | ------ | ------ |
|1 | 01.01.2000 | 20:00 | Al-Hilal | Al-Nassr | 2 | 2 | Semi-final |
- Team1 is always Home team.
- Team2 is always Away team.
- Score1 is Team1 score.
- Score2 is Team2 score.
- Note if the game is special, e.g. Semi-final or Final.
Please contact regarding the data [@alioh](https://twitter.com/alioh)

### Code
## Chosen Algorithm and Rationale​
The Random Forest algorithm was chosen for its robustness, ability to handle categorical and numerical features, and effectiveness in classification tasks. It provides insights into feature importance and performs well on complex datasets like football match records.​
## Model Training, Tuning, and Evaluation​
# Training:
The dataset was split into 80% training and 20% testing sets. The model was trained using team statistics (e.g., average goals, win rates) as predictors and match outcomes (Win, Draw, Loss) as the target.​
# Tuning:
Default hyperparameters, such as 500 trees, were used, ensuring a balance between performance and computational efficiency.​
# Evaluation:
The model was evaluated using the testing set, measuring accuracy, precision, recall, and F1-score to assess classification performance.​
## Model Performance​
The model achieved strong performance metrics, with accuracy exceeding [insert percentage, e.g., 85%].​
Validation confirmed the model's reliability, making it suitable for predicting future match outcomes and analyzing team dynamics.​
