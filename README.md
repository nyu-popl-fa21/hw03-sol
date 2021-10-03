## Homework 3

The deadline for Homework 3 is Monday, October 4, 4pm. The late submission deadline is Monday, October 11, 4pm.

Refer to the homework handout [`hw03.pdf`](hw03.pdf) for details about the assignment. This file provides some information to help you get started with setting up your development environment for the homework assignments.

### Getting the code template

Before you perform the next steps, you first need to create your own
private copy of this git repository. To do so, click on the link
provided in the announcement of this homework assignment on
Brightspace. After clicking on the link, you will receive an email from
GitHub, when your copy of the repository is ready. It will be
available at
`https://github.com/nyu-popl-fa21/hw03-<YOUR-GITHUB-USERNAME>`.
Note that this may take a few minutes.

* Open a browser at `https://github.com/nyu-popl-fa21/hw03-<YOUR-GITHUB-USERNAME>` with your Github username inserted at the appropriate place in the URL.
* Choose a place on your computer for your homework assignments to reside and open a terminal to that location.
* Execute the following git command: <br/>
  ```bash
  git clone https://github.com/nyu-popl-fa21/hw03-<YOUR-GITHUB-USERNAME>.git hw03
  cd hw03
  ```

Please make sure that you clone the copy of your own version of this repository that is identified by your Github username, rather than the template repository.

The problems that you you should solve for this assignment are described in the file `hw03.pdf`.

The assignment consists of a coding part (Problem 1(d,e)) and a non-coding part.

1. Your solutions to the non-coding parts of the assignment should be either put in a plain text file called `solution.txt`, or a PDF file called `solution.pdf`. Scans of clearly readable hand-written solutions are also accepted. Please stick to this naming scheme.

2. The code template for solving Problem 3 is provided in the file 
   ```
   src/main/scala/popl/hw03.scala 
   ``` 
   relative to the root directory of the repository. Follow the instructions in the
   [in-class-code](https://github.com/nyu-popl-fa21/in-class-code)
   repository to import the project into InteliJ (or use your other
   favorite IDE or editor to work on the assignment).

   You can implement the solutions by replacing the `???`
   expressions in `hw03.scala` with your code for the corresponding
   part. Please do not modify the signatures of the given function
   definitions (i.e. do not change the names of these functions, their
   arguments, or their return types).

### Submitting your solution

Once you have completed the assignment, you can submit your solution
by pushing the modified code template to GitHub. This can be done by
opening a terminal in the project's root directory and executing the
following commands in the terminal:

```bash
git add .
git commit -m "solution"
git push
```

You can replace "solution" by a more meaningful commit message.

Refresh your browser window pointing at
```
https://github.com/nyu-popl-fa21/hw03-<YOUR-GITHUB-USERNAME>/
```
and double-check that your solution has been uploaded correctly.

You can resubmit an updated solution anytime by reexecuting the above
git commands. Though, please remember the rules for submitting
solutions after the homework deadline has passed.

### Running JavaScript code

In order to understand how JavaScript's automatic type conversion works, you will have to write simple JavaScript expressions and evaluate them with an existing JavaScript interpreter. We are going to use the Node.js JavaScript runtime for this purpose (version v0.10 or newer will do).

Node.js is a JavaScript platform for writing server applications based on Google Chrome's JavaScript engine. You can download Node.js from

[http://nodejs.org/](http://nodejs.org/)

On most Linux distributions, you should be able to install node.js directly using the package manager of your distribution. Otherwise, follow the installation instructions on the website above.

After installing Node.js, you can run it on the command line by calling it with a JavaScript file as argument like this:

```bash
$ node your-test-file.js
```

Note that if you are on Linux and you install the Node.js version provided by your distribution, the name of the executable may vary.

You may need to enclose expressions in your scripts in print commands to actually see the result of the evaluation, e.g. like this

```javascript
console.log('2' * 3)
```

If you start Node.js without any arguments, it will start a REPL in which you can evaluate expressions interactively, similar to the Scala REPL.

You can also provide a JavaScript expression directly on the command line. For example executing

```bash
$ node -e "'2' * 3" -p
```

will evaluate the expression `'2' * 3` and print the result. 

### Testing your code

To test your code against the provided unit tests you can follow the same instructions as for Homework 1.
