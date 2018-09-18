const express = require('express');
const AWS = require('aws-sdk');
const uuidv4 = require('uuid/v4');
const cors = require('cors');

const S3 = new AWS.S3();
const app = express();
app.use(cors());

const config = {
    port : process.env.PORT || 3003,
    accessKeyId: process.env.ACCESS_KEY_ID,
    secretAccessKey: process.env.SECRET_ACCESS_KEY,
    bucket: process.env.ELM_FILE_S3_BUCKET || 'elm-file-uploads',
    signedUrlExpireSeconds: process.env.SIGNED_URL_EXPIRE_SECONDS || (60 * 5)
};

if (!config.accessKeyId || !config.secretAccessKey) {
    console.error("Please set ACCESS_KEY_ID and SECRET_ACCESS_KEY");
    return;
}

AWS.config.update({ accessKeyId: config.accessKeyId, secretAccessKey: config.secretAccessKey });

app.get('/signed-upload-url', (req, res) => {

    const reference = uuidv4();

    console.log(`Request received (${reference})`);

    const signedUrl = S3.getSignedUrl('getObject', {
        Bucket: config.bucket,
        Key: reference,
        Expires: config.signedUrlExpireSeconds
    });

    console.log(`Signed url generated ${signedUrl} (${reference})`);

    res.json({ signedUrl, reference });

});


app.listen(config.port, () => console.log(`Listening on port ${config.port}`));