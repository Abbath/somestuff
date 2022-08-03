import cv2 as cv
import numpy as np
import matplotlib.pyplot as plt

# split 
def Division_Judge(img, h0, w0, h, w) :
    area = img[h0 : h0 + h, w0 : w0 + w]
    mean = np.mean(area)
    std = np.std(area, ddof = 1)

    if np.sum(area - mean < 2 * std) / (area.shape[0] * area.shape[1]) >= 0.95 :
        return True
    else:
        return False

def Merge(img, h0, w0, h, w) :
    area = img[h0 : h0 + h, w0 : w0 + w]
    _, thresh = cv.threshold(area, 0, 255, cv.THRESH_OTSU | cv.THRESH_BINARY_INV)
    # thresh = cv.adaptiveThreshold(area, 255, cv.BORDER_ISOLATED | cv.BORDER_REPLICATE, cv.THRESH_BINARY_INV, 5, 0)
    img[h0 : h0 + h, w0 : w0 + w] = thresh

    # for row in range(h0, h0 + h) :
    #     for col in range(w0, w0 + w) :
    #         if img[row, col] > 100 and img[row, col] < 200:
    #             img[row, col] = 0
    #         else :
    #             img[row, col] = 255

def Recursion(img, h0, w0, h, w) :
    # If the splitting conditions are met, continue to split 
    if min(h, w) >= 2 and not Division_Judge(img, h0, w0, h, w):
        # Recursion continues to determine whether it can continue to split 
        # Top left square 
        if not Division_Judge(img, h0, w0, int(h / 2), int(w / 2)):
            Recursion(img, h0, w0, int(h / 2), int(w / 2))
        # Upper right square 
        if not Division_Judge(img, h0, w0 + int(w / 2), int(h / 2), int(w / 2)):
            Recursion(img, h0, w0 + int(w / 2), int(h / 2), int(w / 2))
        # Lower left square 
        if not Division_Judge(img, h0 + int(h / 2), w0, int(h / 2), int(w / 2)):
            Recursion(img, h0 + int(h / 2), w0, int(h / 2), int(w / 2))
        # Lower right square 
        if not Division_Judge(img, h0 + int(h / 2), w0 + int(w / 2), int(h / 2), int(w / 2)):
            Recursion(img, h0 + int(h / 2), w0 + int(w / 2), int(h / 2), int(w / 2))
        # Merge
    else:
        Merge(img, h0, w0, h, w)

def Division_Merge_Segmented() :
    img = cv.imread('../P005.jpg')
    img_gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
    hist, bins = np.histogram(img_gray, bins = 256)
    # print(f' Five-pointed star , The ellipse , background , The pixel values of pentagons are :'
    #       f'{", ".join("%s" % pixel for pixel in np.unique(img_gray))}')

    segemented_img = img_gray.copy()
    Recursion(segemented_img, 0, 0, segemented_img.shape[0], segemented_img.shape[1])

    plt.figure(figsize=(12, 4))
    plt.subplot(131), plt.imshow(cv.cvtColor(img, cv.COLOR_BGR2RGB))
    plt.axis('off'), plt.title(f'$input\_image$')
    plt.subplot(132), plt.imshow(img_gray, cmap='gray', vmin = 0, vmax = 255)
    plt.axis('off'), plt.title(f'$gray\_image$')
    plt.subplot(133), plt.imshow(segemented_img, cmap='gray')
    plt.axis('off'), plt.title(f'$segmented\_image$')
    plt.tight_layout()
    plt.show()

if __name__ == '__main__':
    Division_Merge_Segmented()