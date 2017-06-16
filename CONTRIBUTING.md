Thank you for your interest in TouIST! You found a bug, you have a feature
request? And by chance you can code it? Here is the guide to contributing.

Report a bug or a feature
=========================

Simply file an issue [here](https://github.com/touist/touist/issues). Please
check that this bug or feature as not already been submited by someone else;
don't forget to also search into closed issues!

Submit code modifications
=========================

You can fork the project, push your modifications to it and then create a pull
request on the https://github.com/touist/touist repository.

Please make sure all CI builds are green; it might be a bit tricky to pass the
builds sometimes, don't hesitate to ask for help.

FAQ
===

Why are there duplicates of CI build?

- `continuous-integration/appveyor/pr` and `continuous-integration/travis-ci/pr`
  hare builds of temporary commits containing the merge of your branch into
  master
- `continuous-integration/travis-ci/push` and
  `continuous-integration/appveyor/branch` are the builds made out you your
  branch without merging into master
