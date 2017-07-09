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
topBlockClass: "fragment fade-in fade-left "
footerAnchorImage: "../images/nixos-custom.svg"
footerAnchoreHeight: 10%
footerAnchorWidth: 20%
footerAnchorImageMaxWidth: 150%
controls: false
codeBackGroundColor: "#424284"

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

  div.sourceCode>pre.sourceCode{
    background:rgba(00, 00, 00, 0.5);
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

The Nix Package Manager
================================

<section>

There is an excellent explanation of what the **Nix** package manager is on *Sander van der Burg*'s blog
[http://sandervanderburg.blogspot.co.za/2012/11/an-alternative-explaination-of-nix.html]()

and what follows is directly based on that

</section>

<section>

<div class="notes">

[Directly from Sander's blog][an-alternative-explaination-of-nix]

<blockquote>

In short: Nix is a package manager, which is a collection of software tools
to automate the process of installing, upgrading, configuring, and removing
software packages. Nix is different compared to conventional package managers,
because it borrows concepts from purely functional programming languages to make
deployment reliable, reproducible and efficient. The Nix project has been
initiated by Eelco Dolstra as part of his PhD research.

</blockquote>

</div>

Tools to automate **installing**, **upgrading**, **configuring**, and **removing** software packages

| It borrows concepts from
| **purely functional** programming languages
| to make **deployment**
| **reliable**, **reproducible** and **efficient**

Was initiated by **Eelco Dolstra** as part of his *PhD* research

</section>

<section>

<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional programming languages </h4> </div>

Functions are found in many programming languages

But usually not the same as functions in mathematics

| Leibnz' principal
| `𝑥 ＝ 𝑦 ⇒ ⨍⟮𝑥⟯ ＝ ⨍⟮𝑦⟯`
| if two function arguments are identical
| then two function applications are too

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional programming languages are not</h4> </div>
<div style="display:flex;flex-direction:row;align-items:center;">
<div style="width:52%;text-align:left">
This `C` code
```{.c }
int v = 0;

int add(int a)
{
    v = v + a;
    return v;
}

int main()
{
    int p = add(1); /*1*/
    int q = add(1); /*2*/
}
```
</div>
<div>
  <p class="fragment fade-in fade-bottom" style="text-align:left">does not obey the Leibnz' principal</p>
  <p class="fragment fade-in fade-bottom" style="text-align:left">because it allows side-effects to be programmed</p>
  <p class="fragment fade-in fade-bottom" style="text-align:left">meaning it lacks referential transparency</p>
  <p class="fragment fade-in fade-bottom" style="text-align:left">so the function cannot be replaced by its value</p>
</div>
</div>

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional programming languages </h4> </div>

<div style="text-align:left"> Obey the Leibnz' principal </div>

| The result of function **application**
| depends only on its **definition**
| and its **arguments**

Side-effects are not allowed to be programmed (they are pure)

No variable, only identifiers to immutable objects

This all means they are referential transparent

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional programming languages </h4> </div>

<div style="text-align:left"> Are often **lazy** </div>

So expression only get evaluated when needed

Allows *tying the knot* for cyclic data structures while retaining purity

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional programming languages which are lazy</h4> </div>

<div style="text-align:left"> Have **benefits** </div>

Results of **evaluation** can be **cached**

**Evaluation** only happens **when** the result is **required**

Referential transparency gives determinism allowing **parallel evaluation**

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional package management</h4> </div>

Treat deployment of packages as a function in a programming language

</section>

<section>

<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Conventional package management </h4> </div>

<div style="text-align:left"> In conventional package mangers </div>

A *"function"* is representative of imperative languages like *C*

Execution can destructively modify other packages

| Files **installed** in **global locations** (like `/usr/lib`),
| so **dependencies** could be found **without being declared** and
| its **difficult** for **multiple version** of a package to **co-exist**

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional package management</h4> </div>
<div style="text-align:left"> In Nix </div>

| **Build recipes** described with the **Nix Expression DSL**
| (which is a lazy purely functional language)

| A **build** is
| a **function** that describes the build
| being invoked with its **required dependencies as arguments**

Build scripts are run in **isolated environments limiting** chances of **side effects**

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional package management</h4> </div>
<div style="text-align:left"> In Nix </div>

The **result** of each build is **stored** in a **unique separate location**

The **storage** locations are **immutable**

| The storage **identity** is a **cryptographic hash**
| derived from all the **inputs of the function**

**Unique locations** give better **guarantees against implicit dependencies** sneaking through

</section>

<section>
<div style="margin-top:-40px"> <h4 style="font-variant:small-caps;"> Purely functional package management</h4> </div>
<div style="text-align:left"> This means in Nix </div>

You get determinism, the same package is the same where or when ever you install it

You can have multiple versions of packages co-existing

You can safely upgrade any package and always rollback an upgrade

You can in-place upgrade packages

You can actually reason about the state of your packages

</section>

<div class="notes">
[an-alternative-explaination-of-nix]: http://sandervanderburg.blogspot.co.za/2012/11/an-alternative-explaination-of-nix.html
</div>
