const express = require('express');
const uuidv4 = require('uuid/v4');
const cors = require('cors');
const bodyParser = require('body-parser');
const multer = require('multer');
const fs = require('fs');
const path = require('path');
const fileType = require('file-type');

const app = express();
app.use(cors());
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: true}));


const upload = multer({ storage: multer.memoryStorage() });


const config = {
    port: process.env.PORT || 3003,
};

let fileId = 1;
let files = {};
let buffers = {};


app.post('/attachments', upload.single('data'), ({file: { mimetype, buffer }, body: { fileName }}, res) => {


    const reference = uuidv4();

    const file = {
        fileName,
        reference,
        mimetype,
    };

    buffers[reference] = buffer;
    files[reference] = file;

    fileId++;

    res.json(file);
});

app.get('/attachments', (req, res) => {
    res.json(Object.values(files));
});

app.delete('/attachments/:reference', ({params: {reference}}, res) => {

    if (Object.keys(files).indexOf(reference) !== -1) {
        const file = files[reference];
        delete files[reference];
        delete buffers[reference];
        res.json(file);
    } else {
        res.status(404);
        res.send("Unknown file reference");
    }

});

app.get('/attachments/:reference', ({params: {reference}}, res) => {

    const buffer = buffers[reference];

    if (!buffer) {
        res.status(404);
        res.send("Unknown file reference");
        return;
    }

    res.write(buffer,'binary');
    res.end(null, 'binary');

});


app.listen(config.port, () => console.log(`Listening on port ${config.port}`));