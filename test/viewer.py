# A viewer to display custom .tl files. A .tl file is a text file containing a grid of 1s and 0s. A 1 represents a wall, and a 0 represents an empty space. 
# The viewer will display the grid as a black and white image.
# Made this cos the haskell viewer doesn't work 

from PIL import Image
from sys import argv

# Read the file
file = open(argv[1], "r")
lines = file.readlines()
file.close()

# Create the image
img = Image.new("RGB", (len(lines[0]) - 1, len(lines)), "red")
pixels = img.load()

# Fill the image
for y in range(len(lines)):
    for x in range(len(lines[y])):
        if lines[y][x] == "1":
            pixels[x, y] = (0, 0, 0)
        elif lines[y][x] == "0":
            pixels[x, y] = (255, 255, 255)
        
# Display the image
# img.show()

# Save the image
img.save("view_" + argv[1] + ".png")




