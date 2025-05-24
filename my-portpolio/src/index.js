import React from "react";
import ReactDOM from "react-dom";
import "./index.css";

function App() {
  const projects = [
    {
      name: "customer-review-analysis",
      description: null,
      language: null,
      html_url: "https://github.com/gouthampolamreddy/customer-review-analysis",
    },
    {
      name: "ddd-example",
      description: null,
      language: "Java",
      html_url: "https://github.com/gouthampolamreddy/ddd-example",
    },
    {
      name: "exploRe",
      description: "Learn and practice R language",
      language: "R",
      html_url: "https://github.com/gouthampolamreddy/exploRe",
    },
  ];

  return (
    <div className="App">
      <div className="container">
        <h1>Goutham Polamreddy</h1>
        <p>Senior Software Engineer</p>

        <h2>Skills</h2>
        <ul>
          <li>Java</li>
          <li>R</li>
          <li>React</li>
        </ul>

        <h2>Projects</h2>
        {projects.map((project) => (
          <div key={project.name}>
            <h3>{project.name}</h3>
            <p>
              Description:{" "}
              {project.description || "No description available."}
            </p>
            <p>
              Language: {project.language || "Not specified"}
            </p>
            <a
              href={project.html_url}
              target="_blank"
              rel="noopener noreferrer"
            >
              View Project
            </a>
          </div>
        ))}

        <h2>Contact</h2>
        <a
          href="https://github.com/gouthampolamreddy"
          target="_blank"
          rel="noopener noreferrer"
        >
          GitHub Profile
        </a>
      </div>
    </div>
  );
}

const rootElement = document.getElementById("root");
ReactDOM.render(<App />, rootElement);
