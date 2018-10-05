const express = require('express');
const AWS = require('aws-sdk');
const uuidv4 = require('uuid/v4');
const cors = require('cors');
const bodyParser = require('body-parser');

const app = express();
app.use(cors());
app.use(bodyParser.json());

const config = {
    port: process.env.PORT || 3003,
    accessKeyId: process.env.ACCESS_KEY_ID,
    secretAccessKey: process.env.SECRET_ACCESS_KEY,
    bucket: process.env.ELM_FILE_S3_BUCKET || 'elm-file-uploads',
    signedUrlExpireSeconds: process.env.SIGNED_URL_EXPIRE_SECONDS || (6000 * 6000)
};

if (!config.accessKeyId || !config.secretAccessKey) {
    console.error("Please set ACCESS_KEY_ID and SECRET_ACCESS_KEY");
    return;
}

let fileId = 1;
let files = {};

AWS.config.update({
    accessKeyId: config.accessKeyId,
    secretAccessKey: config.secretAccessKey
});

const S3 = new AWS.S3();

app.post('/signed-upload-url', ({body: {contentType, fileName}}, res) => {

    const reference = uuidv4();

    console.log(`Request received (${reference})`);
    console.log(`contentType (${reference})`, contentType);
    console.log(`fileName (${reference})`, fileName);

    const s3Params = {
        Bucket: config.bucket,
        Key: reference,
        ContentType: contentType
    };

    const attachment = {
        id: fileId,
        reference,
        uploadedBy: "Test User",
        fileName,
        tag: null,
        contentType,
        date: new Date,
        softDeleting: false
    };

    fileId++;

    files[reference] = attachment;

    S3.getSignedUrl('putObject', s3Params, (err, signedUrl) => {

        res.json({
            signedUrl: {
                signedUrl,
                reference,
            },
            attachment
        });

    });


});

app.get('/attachments', (req, res) => {
    res.json(Object.values(files));
});

app.put('/attachments/:reference', ({ body, params: { reference }}, res) => {

    if (Object.keys(files).indexOf(reference) !== -1) {
        files[reference] = body;
        res.json(body);
    } else {
        res.status(404);
        res.send("Unknown file reference");
    }

});

app.get('/attachments/:reference', ({params: {reference}}, res) => {

    const file = files[reference];

    if (!file) {
        res.status(404);
        res.send("Unknown file reference");
        return;
    }

    const s3Params = {
        Bucket: config.bucket,
        Key: reference
    };

    res.attachment(file.fileName);
    try {
        S3.getObject(s3Params).createReadStream().pipe(res);
    }
    catch (err) {
        res.setStatus(404).send();
    }


});


app.listen(config.port, () => console.log(`Listening on port ${config.port}`));