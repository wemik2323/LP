import tensorflow as tf
from tensorflow.keras import layers, models
from tensorflow.keras.datasets import mnist

import cv2
import numpy as np

def imgParse(image_path, model):

    # Загрузка и предобработка изображения
    img = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)

    if img is None:
        print(f"Ошибка загрузки изображения {image_path}")
        return
    
    # threshold, перевод в черно-белое
    img = cv2.threshold(img, 128, 255, cv2.THRESH_BINARY)[1]
    
    img = cv2.resize(img, (28, 28))
    img = img.reshape((1, 28, 28, 1)).astype('float32') / 255

    # Предсказание
    predictions = model.predict(img)
    predicted_label = np.argmax(predictions[0])

    # Вывод предсказания
    print(f"Модель предсказывает: {predicted_label}")

# Загрузка данных MNIST
(train_images, train_labels), (test_images, test_labels) = mnist.load_data()

# Предобработка данных
train_images = train_images.reshape((60000, 28, 28, 1)).astype('float32') / 255
test_images = test_images.reshape((10000, 28, 28, 1)).astype('float32') / 255

# Кодирование меток в формат one-hot
train_labels = tf.keras.utils.to_categorical(train_labels)
test_labels = tf.keras.utils.to_categorical(test_labels)

# Создание модели
model = models.Sequential([
    layers.Conv2D(32, (3, 3), activation='relu', input_shape=(28, 28, 1)),
    layers.MaxPooling2D((2, 2)),
    layers.Conv2D(64, (3, 3), activation='relu'),
    layers.MaxPooling2D((2, 2)),
    layers.Conv2D(64, (3, 3), activation='relu'),
    layers.Flatten(),
    layers.Dense(64, activation='relu'),
    layers.Dense(10, activation='softmax')
])

# Компиляция модели
model.compile(optimizer='adam',
            loss='categorical_crossentropy',
            metrics=['accuracy'])
        
# Обучение модели
model.fit(train_images, train_labels, epochs=5, batch_size=1, validation_split=0.2)

# Оценка модели на тестовых данных
test_loss, test_acc = model.evaluate(test_images, test_labels)
print(f"Точность на тестовом наборе: {test_acc}")

imgParse('test.jpg', model)
imgParse('test2.jpg', model)
imgParse('test3.jpg', model)
imgParse('test4.jpg', model)
imgParse('test5.jpg', model)
imgParse('test6.jpg', model)
