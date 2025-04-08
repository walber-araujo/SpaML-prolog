# 🚀 spaML - Spam Classifier in Prolog

spaML is a spam message classifier developed in **Prolog**, using the **Naive Bayes** algorithm to categorize messages as **spam** or **ham** (not spam).

---

## 📌 Development Team
- **Alex Silva**
- **João Brandão**
- **Vinícius Porto**
- **Walber Araújo**

---

## 🛠️ Features
✅ Model training from CSV files  
✅ Reuse of previously trained models  
✅ Removal of user-trained models (default models cannot be deleted)  
✅ Manual training  
✅ Classification of individual messages  
✅ Display of accuracy metrics of trained models  
✅ Interactive interface via command line (CLI)  

---

## 📁 Project Structure

```
spaML/
├── README.md
├── app
│   └── main.pl
├── data
│   ├── models
│   │   └── models.json
│   └── train_data
│       ├── SMSSpamCollection.csv
│       └── short_messages_formatted.csv
├── src
│   ├── CLI.pl
│   ├── Classifier.pl
│   ├── Intro.pl
│   ├── Metric.pl
│   ├── Model.pl
│   ├── ModelTest.pl
│   ├── Preprocessing.pl
│   ├── Training.pl
│   └── Utils.pl
└── test
    ├── ClassifierTest.plt
    ├── Main.plt
    ├── ModelTest.plt
    ├── PreprocessingTest.plt
    └── TrainingTest.plt
```

---

## 📋 Requirements

---

## 🚀 How to Run

1️⃣ Clone the repository:
```sh
git clone https://github.com/walber-araujo/SpaML-prolog.git
```

2️⃣ Run:

Linux/MacOS:

```sh
swipl -t halt app/main.pl
```

Windows:

```sh

```


### 🧪 Tests & linter

To run the linter:
```sh
swipl -s lint.pl
```

To run the tests:
```sh
swipl -t halt test/Main.plt
```

---

## 🖥️ Usage

### 📌 CLI Options:
| Option | Action |
|---------|--------|
| **1** | Reuse previous models |
| **2** | Add new model |
| **3** | Remove a model |
| **4** | Train model manually |
| **5** | Classify individual messages using the default model |
| **6** | Show results with accuracy rates |
| **7** | Exit |

### 📂 CSV file format
CSV files used for training must follow this format:
```
label,message
ham,Hello, how are you?
spam,You won a free prize! Click here!
```
> Where `label` can be **ham** or **spam**.

---

## 📜 License


---

💡 **Contributions are welcome!** Feel free to open **issues** and send **pull requests**. 😊
