const express = require('express');
const app = express();
const PORT = 3000;

app.get('/', (req, res) => {
  console.log('Received request');
  res.send('Hello, World!');
});

app.listen(PORT, () => {
  console.log(`Server running at http://localhost:${PORT}`);
});
