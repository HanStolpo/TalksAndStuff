---
title: Adventures with the purely functional OS NixOS
author: Handré Stolp
date: November 23, 2015
slideLevel: 1
incremental: false
slideVariant: RevealJsSlides
autoStretchCode: false
designWidth: 1024
designHeight: 768
margin: 0.1
minScale: 0.1
maxScale: 3
backGroundImage: "../images/nix-wallpaper-mosaic-blue.png"
backGroundSize: "110% 110%"
backGroundColor: "#424284"
theme: night
slideState: "customize-slides"
topParagraphClass: "fragment fade-in fade-left"
footerAnchorImage: "../images/nixos-custom.svg"
footerAnchoreHeight: 10%
footerAnchorWidth: 20%
footerAnchorImageMaxWidth: 150%

---

<style>
  .customize-slides h1{
    text-transform:initial;
    font-size: 200%;
  }
  section.move-up>div:nth-child(2){
    margin-top: -100px;
  }
  section.move-up-sub>div:first-child{
    margin-top: -50px;
  }
  .reveal section img.transparent-background{
    background:transparent;
    border:none;
    box-shadow:none;
  }
</style>


<section data-background-color="#424284" data-background-image="../images/nix-wallpaper-mosaic-blue.png" data-background-size="110% 110%">
  <div> <h1 style="text-transform:initial;font-size:200%"> Adventures with the purely functional OS </h1>
  </div>
  <div> <image class="transparent-background" src="../images/nixos.svg" />  </div>
</section>


NixOS Ecology  <span style="font-variant:small-caps;font-size:70%;line-height:0.1;vertical-align:text-top"> <br/> tersely </span> {.move-up}
===================

**NixOS** is a Linux distribution based on the Nix package manager

**Nix** package supports Linux, OS X and FreeBSD

**Nix** uses the purely functional **Nix Expression** language to define packages

**NixOps** uses **Nix** to deploy **NixOS** based infrastructure to Amazon Web Services, Google Cloud Platform, VirtualBox, Hetzner and NixOS containers

**DisNix** uses **Nix** to deploy distributed services

If I told you with NixOS
========================

<section>

For those that come from *Windows*, there are no software installers

For those from *OS X*, there is no *App Store*

For those from *Linux*, not *FHS* compliant
<br/> no `/sbin` `/lib` `/lib64` `/usr/local` `/usr/lib`  ...
<br/> `/etc` off limits
<br/> only `/bin/sh` and `/usr/bin/env`

In general you can't install arbitrary applications or libraries from the internet

</section>

<section>

You have to learn the *Nix Expression* language

You may have to learn a little bit about purely
functional programming

If you get stuck you can't just get the answer on *AskUbuntu*

You have to add packages to the package manager

</section>

You might ask  {data-state=""}
============

<div>
![](../images/you-mad.gif "You mad gif")
</div>

The answer is
=============

<div style="display: flex">
<div>
We do these things not because they are easy, but because they are hard
</div>
<div>
![](../images/JFK.svg "JFK"){width=500px .transparent-background}
</div>
</div>

<div class="fragment fade-in fad-up" style="display: flex">
<div>
![](../images/earthtothemoon-1922.svg  "earthtothemoon-1922"){width=250px .transparent-background style="position:absolute; top:40%; left:20%"}
</div>
<div style="width:50%;margin-left:30%;">
and because it makes the impossibly difficult easily possible
</div>
</div>


Lets get down to brass tacks  {data-state=""}
============

Purely functional package manager
================================
