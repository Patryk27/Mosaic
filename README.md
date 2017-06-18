# Mosaic
*(old abandoned project)*

Mosaic picture generator.

# What is this project?

This application can generate a mosaic of given picture using given other, smaller pictures.

I've made it purely for fun because I had a catalogue of like 2k memes and I was curious what can I do with them except spamming friends.

# How it works?

It scans given directory for pictures, scales them to given size (eg. `20x20`px), generates average colour of each one and then tries to match a picture for each pixel of given main image.

An image cache is generated for the scaled images so that the next time application runs, it does not have to scale them again.

I've got very bad results using RGB and thus everything is internally transformed to LAB color space which seems to do a decent job.

# It does not work for me.

Sorry to hear that but like I said before - it's and old project created solely for fun, no testing has been done to ensure it works anywhere beside my own computer.

# Too much text, show me something.

## Cat 1

![Cat 1 - before](/docs/imgs/cat-1.jpg)
![Cat 1 - after](/docs/imgs/cat-1-rendered.jpg)

## Cat 2

![Cat 2 - before](/docs/imgs/cat-2.jpg)
![Cat 2 - after](/docs/imgs/cat-2-rendered.jpg)

## Rainbow

![Rainbow - before](/docs/imgs/rainbow.jpg)
![Rainbow - after](/docs/imgs/rainbow-rendered.jpg)
