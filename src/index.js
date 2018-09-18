import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));


app.ports.readFileContent.subscribe((file) => {


    (function ({ id, data: { name } }) {

        const element = document.getElementById('file-upload-input');

        if (!element && element.files) {
            console.error("Invalid element"); s
            return;
        }

        for (var i = 0; i < element.files.length; i++) {

            let file = element.files[i];

            if (file.name === name) {

                var reader = new FileReader();

                reader.onload = (function ({ target: { result } }) {

                    // We call the `fileContentRead` port with the file data
                    // which will be sent to our Elm runtime via Subscriptions.
                    app.ports.fileContentRead.send({
                        id,
                        data: {
                            contents: result,
                            fileName: file.name
                        }
                    });

                });

                // Connect our FileReader with the file that was selected in our `input` node.
                reader.readAsDataURL(file);

            }

        }

    })(JSON.parse(file))



});

app.ports.browseClick.subscribe(() => {
    const element = document.getElementById('file-upload-input');
    if (element) {
        element.click();
    }
});

registerServiceWorker();