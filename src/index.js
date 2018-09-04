import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));


app.ports.browseClick.subscribe(() => {
    const element = document.getElementById('file-upload-input');
    if (element) {
        element.click();
    }
});

registerServiceWorker();