import os

import cv2


img_dir = os.path.join('..', 'data', 'images')

for f_name in os.listdir(img_dir):
	if not f_name.startswith('.'):
		img = cv2.imread(os.path.join(img_dir, f_name))
		img = cv2.resize(img, (640, 480))
		cv2.imwrite(os.path.join(img_dir, f_name), img)
